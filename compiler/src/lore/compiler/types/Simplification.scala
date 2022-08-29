package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.VectorMapExtension

object Simplification {

  /**
    * Constructs a type of `kind`, either a sum type or intersection type, from the given parts. This includes
    * flattening, combining types according to their variance, and taking only the most general/specific types.
    *
    * If a subterm is covariant, the type candidates are combined with the constructor as `tpe` (sum or intersection).
    * If a subterm is contravariant, the opposite constructor is chosen (intersection or sum). Invariant subterms
    * cannot be combined, meaning types with invariant subterms are left as is.
    *
    * The following kinds of types will be simplified: Tuples of the same size, functions, lists, shapes (intersection
    * types only), and declared types of the same schema.
    *
    * TODO (maps): Before this can be implemented for maps, we have to clear up the variance of maps.
    */
  def construct(kind: Kind, parts: Vector[Type]): Type = {
    val flattened = flatten(parts, kind)
    val combined = combine(flattened, kind)
    val relevants = filterRelevantTypes(combined, kind)

    if (relevants.size == 1) {
      relevants.head
    } else {
      kind match {
        case Kind.Sum => SumType(relevants)
        case Kind.Intersection => IntersectionType(relevants)
        case _ => invalidKind(kind)
      }
    }
  }

  private def flatten(parts: Vector[Type], kind: Kind): Vector[Type] = {
    parts.flatMap {
      case SumType(parts) if kind == Kind.Sum => parts
      case IntersectionType(parts) if kind == Kind.Intersection => parts
      case t => Vector(t)
    }
  }

  private def combine(parts: Vector[Type], kind: Kind): Vector[Type] = {
    val constructSum = (parts: Vector[Type]) => SumType.construct(parts)
    val constructIntersection = (parts: Vector[Type]) => IntersectionType.construct(parts)
    val (constructCovariant, constructContravariant) = kind match {
      case Kind.Sum => (constructSum, constructIntersection)
      case Kind.Intersection => (constructIntersection, constructSum)
      case _ => invalidKind(kind)
    }

    var tuplesBySize = Map.empty[Int, Vector[TupleType]]
    var functions = Vector.empty[FunctionType]
    var lists = Vector.empty[ListType]
    var shapes = Vector.empty[ShapeType]
    var declaredTypesBySchema = Map.empty[DeclaredSchema, Vector[DeclaredType]]
    var result = Vector.empty[Type]

    parts.foreach {
      case tuple@TupleType(elements) => tuplesBySize = tuplesBySize.appended(elements.length, tuple)
      case function: FunctionType => functions = functions :+ function
      case list: ListType => lists = lists :+ list
      case shape: ShapeType if kind == Kind.Intersection => shapes = shapes :+ shape
      case dt: DeclaredType if !dt.schema.hasInvariantTypeParameters => declaredTypesBySchema = declaredTypesBySchema.appended(dt.schema, dt)
      case t => result = result :+ t
    }

    // (A, B) | (C, D) :=: (A | C, B | D)
    // (A, B) & (C, D) :=: (A & C, B & D)
    tuplesBySize.foreach {
      case (_, Vector(tuple)) => result = result :+ tuple
      case (_, tuples) => result = result :+ TupleType(tuples.map(_.elements).transpose.map(constructCovariant))
    }

    // (A => B) | (C => D) :=: (A & C) => (B | D)
    // (A => B) & (C => D) :=: (A | C) => (B & D)
    if (functions.nonEmpty) {
      result = result :+ FunctionType(
        Type.tupled(constructContravariant(functions.map(_.input))),
        constructCovariant(functions.map(_.output))
      )
    }

    // [A] | [B] :=: [A | B]
    // [A] & [B] :=: [A & B]
    if (lists.nonEmpty) {
      result = result :+ ListType(constructCovariant(lists.map(_.element)))
    }

    // Note that shape type combining is only performed for intersection types, but this has already been checked
    // during categorization. Hence, `shapes` is only non-empty if `tpe` is an intersection type.
    // { name: A } & { name: B } & { health: Int }  ==  { name: A & B, health: Int }
    if (shapes.nonEmpty) {
      result = result :+ ShapeType.combine(shapes)
    }

    // Given a declared type `D[+X, -Y]`:
    // D[A, B] | D[C, D] :=: D[A | C, B & D]
    // D[A, B] & D[C, D] :=: D[A & C, B | D]
    declaredTypesBySchema.foreach {
      case (_, Vector(dt)) => result = result :+ dt
      case (schema, declaredTypes) =>
        val allArguments = declaredTypes.map(_.typeArguments).transpose
        val combinedArguments = schema.typeParameters.zip(allArguments).map {
          case (parameter, arguments) => parameter.variance match {
            // We've already established that the declared type has no invariant parameters during categorization.
            case Variance.Covariant => constructCovariant(arguments)
            case Variance.Contravariant => constructContravariant(arguments)
            case _ => throw CompilationException(s"At this stage, a type parameter may not be invariant. Type schema: $schema.")
          }
        }
        schema.instantiate(combinedArguments) match {
          case Some(dt) => result = result :+ dt
          case None =>
            // If the schema cannot be instantiated with the combined arguments, for example because lower or upper
            // bounds are violated, we default to the uncombined declared types.
            result = result ++ declaredTypes
        }
    }

    result
  }

  private def filterRelevantTypes(parts: Vector[Type], kind: Kind): Set[Type] = {
    kind match {
      case Kind.Sum => parts.filterNot(t => parts.exists(t < _)).toSet
      case Kind.Intersection => parts.filterNot(t => parts.exists(_ < t)).toSet
      case _ => invalidKind(kind)
    }
  }

  private def invalidKind(kind: Kind): Nothing = {
    throw CompilationException(s"Cannot construct a sum or intersection type from kind $kind.")
  }

}
