package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeExtensions._
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.{OptionTuple2Extension, OptionVectorExtension, VectorExtension}

object LeastUpperBound {

  /**
    * Calculates the least upper bound of two types, which is the most specific (super-)type that values of both
    * t1 and t2 could be assigned to. If t1 and t2 don't share a supertype, we return t1 | t2 or Any, depending on
    * the defaultToSum setting.
    */
  def leastUpperBound(t1: Type, t2: Type)(implicit registry: Registry): Type = lubDefaultToSum(t1, t2)

  def leastUpperBound(types: Vector[Type])(implicit registry: Registry): Type = {
    types.foldLeft(BasicType.Nothing: Type)(LeastUpperBound.leastUpperBound)
  }

  private[types] def lubDefaultToSum(t1: Type, t2: Type)(implicit registry: Registry): Type = ConfigurableLub(defaultToSum = true).apply(t1, t2)
  private[types] def lubNoDefaultSum(t1: Type, t2: Type)(implicit registry: Registry): Type = ConfigurableLub(defaultToSum = false).apply(t1, t2)

  private case class ConfigurableLub(defaultToSum: Boolean)(implicit registry: Registry) {

    /**
      * When deciding the LUB of a type that contains a sum type, we should not allow the LUB of the inner sum type to
      * fallback to a sum type. Otherwise, the LUB never converges and just produces long chains of sum types. This is
      * the reason why we're using lubNoDefaultSum to handle part LUBs of intersection types, sum types, and type
      * variable bounds.
      *
      * TODO: I don't think we should pass on settings in cases where child types are lubbed. The setting should not
      *       propagate through several layers. Otherwise, if we lub two shape types contained in a sum type, we
      *       suddenly cannot fallback to the sum of two property types because the surrounding sum type's LUB is
      *       calculated with `defaultToSum = false`.
      */
    def apply(t1: Type, t2: Type): Type = {
      (t1, t2) match {
        case (t1, t2) if t1 <= t2 => t2
        case (t1, t2) if t2 <= t1 => t1

        // The least upper bound of two type variables is simply the LUB of their upper bounds (at compile-time, of
        // course). All values assignable to either v1 or v2 for ANY actual types that the variables can represent at
        // run-time must be described by the result of the LUB. This can only be either the sum of the two variables,
        // V1 | V2, or preferably some kind of common upper bound.
        case (v1: TypeVariable, v2: TypeVariable) => lubNoDefaultSum(v1.upperBound, v2.upperBound).fallbackIfAny(v1, v2)
        // The least upper bound of a type variable and a type (which is not itself a type variable) is similar to the
        // treatment above, only with the idea that we treat the type t2 as its own "upper bound".
        case (v1: TypeVariable, t2) => lubNoDefaultSum(v1.upperBound, t2).fallbackIfAny(v1, t2)
        case (t1, v2: TypeVariable) => apply(v2, t1)

        // Intersection types can lead to a number of candidates. That is, if we have types A & B and C & D, we have
        // the following candidates: LUB(A, C), LUB(A, D), LUB(B, C), LUB(B, D). That's because any part of an
        // intersection type is its supertype, so the least upper bound of two intersection types would be a combination
        // of one part from t1 and another part from t2.
        // From the candidates, we choose the most specific types, meaning those who don't have a strict subtype in the
        // candidate list. This part of the algorithm is taken care of by IntersectionType.construct, as intersection
        // types are specifically simplified according to this rule.
        // We have to use lubNoDefaultSum for the candidate LUBs because we have to be able to decide which candidates
        // are good and which aren't. Take for example `LUB(Cat & Sick, Penguin & Healthy)`. What we want is a type
        // `Animal & Status`. But if we allow `LUB(Sick, Penguin)` to default to a sum type, we get `Sick | Penguin`
        // which is then irreducible and leads to weird types like `Animal & (Cat | Healthy) & (Sick | Penguin) & Status`.
        // Defaulting to Any immediately removes these parts from the intersection type, so we only include types in
        // the intersection which add useful information.
        case (t1: IntersectionType, t2: IntersectionType) =>
          val candidates = for { c1 <- t1.parts; c2 <- t2.parts } yield lubNoDefaultSum(c1, c2)
          IntersectionType.construct(candidates)
        case (t1: IntersectionType, _) => apply(t1, IntersectionType(Set(t2)))
        case (_, t2: IntersectionType) => apply(t2, t1)

        // In this step, we always try to join sum types. The ordering in the match is important: if we compare an
        // intersection type with a sum type, we want the intersection handling above to be executed by the match.
        // TODO: Can't we refine this? If we join each sum immediately, we might lose opportunity in cases where
        //       the left and right type taken together provide enough information for a good decision. For
        //       example, given structs s1, s2 and shape type s3, LUB(s1 | s2, s3) could lead to a shape type
        //       that correctly combines properties from s1, s2, and s3. But if we join s1 and s2 immediately,
        //       resulting for example in a trait, we cannot further LUB that with the shape type s3.
        case (t1: SumType, _) =>
          lubNoDefaultSum(
            t1.join,
            t2 match { case st: SumType => st.join; case t => t }
          ).fallbackIfAny(t1, t2)
        case (_, t2: SumType) => apply(t2, t1)

        // In case of tuple types, we can decide the closest common supertype element by element.
        case (TupleType(left), TupleType(right)) if left.size == right.size => TupleType(left.zip(right).map((apply _).tupled))

        // The function output types can be decided as a LUB, but the input types are contravariant. So they have to be
        // merged as the reverse of a sum type: an intersection type.
        case (f1: FunctionType, f2: FunctionType) =>
          FunctionType(
            Type.tupled(IntersectionType.construct(Vector(f1.input, f2.input))),
            apply(f1.output, f2.output)
          )

        case (d1: DeclaredType, d2: DeclaredType) => handleDeclaredTypes(d1, d2)

        // Lists simply wrap the lubbed types.
        case (ListType(e1), ListType(e2)) => ListType(apply(e1, e2))

        // Maps also wrap the lubbed types, but for both key and value types.
        case (MapType(k1, v1), MapType(k2, v2)) => MapType(apply(k1, k2), apply(v1, v2))

        // The LUB of two shape types or a struct type and a shape type are the properties common to the two types.
        // Note that we don't produce a shape type as the LUB of two structs because that case is correctly handled
        // by querying the declared type hierarchy.
        // TODO: Big problem: Let's say we have a LUB of two structs s1, s2 and a shape type s3. s1 and s2 extend
        //       trait t1. If we LUB s1 and s2 first, we get t1. Then LUB(t1, s3) = Any, because traits don't have
        //       properties. Whereas if we first have LUB(s1, s3) = { a: A, b: B } = s4 and the LUB(s2, s4) = { a: A },
        //       we would get another result. The correct one.
        //       Clearly, if we are lubbing more than one struct with at least one shape type, we want to default to
        //       structural typing. Without any shape type present, we want to consult the type hierarchy.
        case (s1: ShapeType, s2: ShapeType) =>
          val properties = ShapeType.common(s1, s2).map {
            case (p1, p2) => ShapeType.Property(p1.name, apply(p1.tpe, p2.tpe))
          }
          ShapeType(properties)
        case (s1: StructType, s2: ShapeType) => apply(s1.asShapeType, s2)
        case (s1: ShapeType, s2: StructType) => apply(s1, s2.asShapeType)

        // Handle Int and Real specifically.
        case (BasicType.Int, BasicType.Real) => BasicType.Real
        case (BasicType.Real, BasicType.Int) => BasicType.Real

        // In any other case, we can say that t1 and t2 are inherently incompatible. For example, a tuple type and a
        // struct type could never be unified in this sense, and neither could two tuple types with different lengths.
        // Hence we return the default.
        case _ => fallback(t1, t2)
      }
    }

    /**
      * For declared types, the LUB's possible supertype schemas are calculated by the declared type hierarchy. Then
      * we instantiate these schemas according to `findSupertype` for both d1 and d2. Since there are two instances, we
      * combine the two type arguments for each parameter according to its variance. If a type argument cannot be
      * combined, we disregard the specific supertype schema, as it cannot become a LUB of d1 and d2 if type arguments
      * cannot be unified.
      *
      * Once all schemas have been instantiated, we form an intersection type with all of them. If there are no
      * supertype schemas or no valid instances, we return the fallback type instead.
      *
      * We specifically don't default to a shape type (which would be possible if we LUB two structs) because we only
      * want to produce shape types if a shape type is already part of the types to LUB. This "disables" structural
      * typing until the programmer explicitly says that they want it.
      */
    private def handleDeclaredTypes(d1: DeclaredType, d2: DeclaredType): Type = {
      val result = if (d1.schema == d2.schema) {
        // If the schemas of the two types are the same, we try to combine the type arguments to get a common supertype
        // with the same schema.
        declaredTypeHandleCandidate(d1, d2)
      } else {
        val supertypeSchemas = registry.declaredTypeHierarchy.leastCommonSuperschemas(d1.schema, d2.schema).filterType[DeclaredSchema]
        declaredTypeHandleSupertypeSchemas(supertypeSchemas, d1, d2)
      }
      result.getOrElse(fallback(d1, d2))
    }

    private def declaredTypeHandleCandidate(c1: DeclaredType, c2: DeclaredType): Option[Type] = {
      if (c1.schema != c2.schema) {
        throw CompilationException(s"The candidates $c1 and $c2 must have the same schema.")
      }

      c1.typeArguments.zip(c2.typeArguments).zip(c1.schema.parameters)
        .map { case ((a1, a2), parameter) => val result = combineTypeArguments(parameter, a1, a2); println(s"combine $a1 and $a2 into $result"); combineTypeArguments(parameter, a1, a2) }
        .sequence
        .flatMap(c1.schema.instantiate)
        .orElse {
          // If the two candidates c1 and c2 have incompatible type arguments, they cannot be a common supertype.
          // However, c1 and c2 might instead have a common supertype that does not suffer from the incompatible type
          // arguments.
          declaredTypeHandleSupertypeSchemas(c1.schema.declaredSuperschemas, c1, c2)
        }
    }

    private def declaredTypeHandleSupertypeSchemas(schemas: Vector[DeclaredSchema], d1: DeclaredType, d2: DeclaredType): Option[Type] = {
      val supertypes = schemas.flatMap(declaredTypeHandleSupertypeSchema(_, d1, d2))
      if (supertypes.nonEmpty) Some(IntersectionType.construct(supertypes)) else None
    }

    private def declaredTypeHandleSupertypeSchema(supertypeSchema: DeclaredSchema, d1: DeclaredType, d2: DeclaredType): Option[Type] = {
      (d1.findSupertype(supertypeSchema), d2.findSupertype(supertypeSchema))
        .sequence
        .flatMap { case (s1, s2) => declaredTypeHandleCandidate(s1, s2) }
    }

    private def combineTypeArguments(parameter: TypeVariable, a1: Type, a2: Type): Option[Type] = {
      parameter.variance match {
        case Variance.Covariant => Some(lubDefaultToSum(a1, a2))
        case Variance.Contravariant => Some(IntersectionType.construct(Vector(a1, a2)))
        case Variance.Invariant => if (a1 == a2) Some(a1) else None
      }
    }

    /**
      * The fallback LUB, which is either Any or t1 | t2 depending on the settings.
      */
    private def fallback(t1: Type, t2: Type) = if (defaultToSum) SumType.construct(Vector(t1, t2)) else BasicType.Any

    private implicit class FallbackIfAny(tpe: Type) {
      def fallbackIfAny(t1: Type, t2: Type): Type = tpe match {
        case BasicType.Any => fallback(t1, t2)
        case t => t
      }
    }

  }

}
