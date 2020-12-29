package lore.compiler.types

import lore.compiler.semantics.Registry
import lore.compiler.types.TypeExtensions._

object LeastUpperBound {

  def leastUpperBound(types: Vector[Type])(implicit registry: Registry): Type = types.foldLeft(BasicType.Nothing: Type)(LeastUpperBound.leastUpperBound)

  def leastUpperBound(t1: Type, t2: Type)(implicit registry: Registry): Type = lubDefaultToSum(t1, t2)
  private[types] def lubDefaultToSum(t1: Type, t2: Type)(implicit registry: Registry): Type = configurableLub(defaultToSum = true)(t1, t2)
  private[types] def lubNoDefaultSum(t1: Type, t2: Type)(implicit registry: Registry): Type = configurableLub(defaultToSum = false)(t1, t2)

  /**
    * Calculates the least upper bound of two types, which is the most specific (super-)type that values of both
    * t1 and t2 could be assigned to. If t1 and t2 don't share a supertype, we return t1 | t2 or Any, depending on
    * the defaultToSum setting.
    *
    * In general, we can say that if t1 <= t2, then t2 is the solution to this problem. This is easy to see: We
    * want the most specific common supertype. What could be more specific than one of the types themselves?
    *
    * When deciding the LUB of a type that contains a sum type, we should not allow the LUB of the inner sum type to
    * fallback to a sum type. Otherwise, the LUB never converges and just produces long chains of sum types. This is
    * the reason why we're using lubNoDefaultSum to handle part LUBs of intersection types, sum types, and type
    * variable bounds.
    */
  private def configurableLub(defaultToSum: Boolean)(t1: Type, t2: Type)(implicit registry: Registry): Type = {
    /**
      * The fallback LUB, which is either Any or t1 | t2 depending on the settings.
      */
    def fallback = if (defaultToSum) SumType.construct(Set(t1, t2)) else BasicType.Any

    // TODO: I don't think we should pass on settings in cases where child types are lubbed. The setting should not
    //       propagate through several layers. Otherwise, if we lub two shape types contained in a sum type, we
    //       suddenly cannot fallback to the sum of two property types because the surrounding sum type's LUB is
    //       calculated with `defaultToSum = false`.
    def lubPassOnSettings = configurableLub(defaultToSum) _

    implicit class FallbackIfAny(tpe: Type) {
      def fallbackIfAny: Type = tpe match {
        case BasicType.Any => fallback
        case t => t
      }
    }

    (t1, t2) match {
      // First of all, handle subtypes as outlined above. These cases trivially cover Any and Nothing.
      case (t1, t2) if t1 <= t2 => t2
      case (t1, t2) if t2 <= t1 => t1

      // The least upper bound of two type variables is simply the LUB of their upper bounds (at compile-time, of
      // course). All values assignable to either v1 or v2 for ANY actual types that the variables can represent at
      // run-time must be described by the result of the LUB. This can only be either the sum of the two variables,
      // V1 | V2, or preferably some kind of common upper bound.
      case (v1: TypeVariable, v2: TypeVariable) => lubNoDefaultSum(v1.upperBound, v2.upperBound).fallbackIfAny
      // The least upper bound of a type variable and a type (which is not itself a type variable) is similar to the
      // treatment above, only with the idea that we treat the type t2 as its own "upper bound".
      case (v1: TypeVariable, t2) => lubNoDefaultSum(v1.upperBound, t2).fallbackIfAny
      case (t1, v2: TypeVariable) => lubPassOnSettings(v2, t1)

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
      // TODO: Is this algorithm the same when applied via reduction (reducing to the LUB by pairs) and to a list
      //       of intersection types? That is, does the following hold: LUB(LUB(A, B), C) = LUB(A, B, C)?
      case (t1: IntersectionType, t2: IntersectionType) =>
        // TODO: When calling lubNoDefaultSum, should we pass the "no default to sum" setting down the call tree,
        //       actually? Or should this be confined to the immediate concern of having a correct candidate algorithm?
        //       We need to test this with more complex examples, though they should still make sense, of course.
        val candidates = for {c1 <- t1.parts; c2 <- t2.parts} yield lubNoDefaultSum(c1, c2)
        IntersectionType.construct(candidates)
      case (t1: IntersectionType, _) => lubPassOnSettings(t1, IntersectionType(Set(t2)))
      case (_, t2: IntersectionType) => lubPassOnSettings(t2, t1)

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
        ).fallbackIfAny
      case (_, t2: SumType) => lubPassOnSettings(t2, t1)

      // In case of product types, we can decide the closest common supertype element by element.
      case (ProductType(left), ProductType(right)) if left.size == right.size => ProductType(left.zip(right).map(lubPassOnSettings.tupled))

      // For declared types, the LUB is calculated by the type hierarchy. If the result would be Any, we return
      // the fallback type instead.
      // We specifically don't default to a shape type (which would be possible if we LUB two structs) because we only
      // want to produce shape types if a shape type is already part of the types to LUB. This "disables" structural
      // typing until the programmer explicitly says that they want it.
      case (d1: DeclaredType, d2: DeclaredType) => registry.declaredTypeHierarchy.leastCommonSupertype(d1, d2).fallbackIfAny

      // Lists simply wrap the lubbed types.
      case (ListType(e1), ListType(e2)) => ListType(lubPassOnSettings(e1, e2))

      // Maps also wrap the lubbed types, but for both key and value types.
      case (MapType(k1, v1), MapType(k2, v2)) => MapType(lubPassOnSettings(k1, k2), lubPassOnSettings(v1, v2))

      // The LUB of two shape types or a struct type and a shape type are the properties common to the two types.
      // Note that we don't produce a shape type as the LUB of two structs because that case is correctly handled
      // by querying the declared type hierarchy.
      // TODO: Big problem: Let's say we have a LUB of two structs s1, s2 and a shape type s3. s1 and s2 implement
      //       trait t1. If we LUB s1 and s2 first, we get t1. Then LUB(t1, s3) = Any, because traits don't have
      //       properties. Whereas if we first have LUB(s1, s3) = { a: A, b: B } = s4 and the LUB(s2, s4) = { a: A },
      //       we would get another result. The correct one.
      //       Clearly, if we are lubbing more than one struct with at least one shape type, we want to default to
      //       structural typing. Without any shape type present, we want to consult the type hierarchy.
      case (s1: ShapeType, s2: ShapeType) =>
        val properties = s1.common(s2).map {
          case (p1, p2) => ShapeType.Property(p1.name, lubPassOnSettings(p1.tpe, p2.tpe))
        }
        ShapeType(properties)
      case (s1: StructType, s2: ShapeType) => lubPassOnSettings(s1.asShapeType, s2)
      case (s1: ShapeType, s2: StructType) => lubPassOnSettings(s1, s2.asShapeType)

      // Handle Int and Real specifically.
      case (BasicType.Int, BasicType.Real) => BasicType.Real
      case (BasicType.Real, BasicType.Int) => BasicType.Real

      // In any other case, we can say that t1 and t2 are inherently incompatible. For example, a product type and
      // a struct type could never be unified in this sense, and neither could two product types with different
      // lengths. Hence we return the default.
      case _ => fallback
    }
  }

}
