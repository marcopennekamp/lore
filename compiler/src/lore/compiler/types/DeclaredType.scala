package lore.compiler.types

import lore.compiler.types.TypeVariable.Variance
import lore.compiler.utils.CollectionExtensions.VectorExtension

trait DeclaredType extends NamedType {

  /**
    * The schema this declared type was instantiated from.
    */
  def schema: DeclaredSchema

  override def name: String = schema.name

  /**
    * The type arguments this declared type has been instantiated with.
    */
  def assignments: TypeVariable.Assignments

  lazy val typeArguments: Vector[Type] = schema.parameters.map(Type.substitute(_, assignments))

  /**
    * All direct declared supertypes of the declared type. Any type arguments of the declared type are found in their
    * instantiated versions.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = schema.declaredSupertypes.map(Type.substitute(_, assignments).asInstanceOf[DeclaredType])

  /**
    * The inherited shape type of the declared type is derived from the schema's inherited shape type, but with all
    * type parameters instantiated given the current type arguments.
    */
  lazy val inheritedShapeType: ShapeType = Type.substitute(schema.inheritedShapeType, assignments).asInstanceOf[ShapeType]

  /**
    * The declared type viewed as a compile-time shape type. By default, this is equal to the [[inheritedShapeType]].
    * Structs, however, implement `asShapeType` based on their properties.
    */
  def asShapeType: ShapeType = inheritedShapeType

  /**
    * Finds a supertype of this type (or this type itself) that has the given schema.
    *
    * If this type's schema has multiple parameterized inheritance, the result's type arguments are each a combination
    * of all occurring type argument candidates according to variance:
    *
    *   - If a type parameter is covariant, the resulting type argument is the intersection type of all candidates.
    *   - If a type parameter is contravariant, the resulting type argument is the sum type of all candidates.
    *   - If a type parameter is invariant, all candidates must be equal. Otherwise, `findSupertype` results in None.
    *
    * For example, if we have a type `Cage[+A]` with the candidates `Cage[Animal]`, `Cage[Fish]`, and `Cage[Unicorn]`,
    * the resulting declared type will be `Cage[Fish & Unicorn]`.
    */
  def findSupertype(supertypeSchema: DeclaredSchema): Option[DeclaredType] = {
    if (!schema.hasMultipleParameterizedInheritance) {
      if (this.schema == supertypeSchema) Some(this)
      else declaredSupertypes.firstDefined(_.findSupertype(supertypeSchema))
    } else {
      def collect(dt: DeclaredType): Vector[DeclaredType] = {
        if (dt.schema == supertypeSchema) Vector(dt)
        else dt.declaredSupertypes.flatMap(collect)
      }

      collect(this) match {
        case Vector() => None
        case Vector(candidate) => Some(candidate)
        case candidates =>
          val combinedArguments = (0 until supertypeSchema.arity).toVector.map { index =>
            val parameter = supertypeSchema.parameters(index)
            val arguments = candidates.map(_.typeArguments(index))
            parameter.variance match {
              case Variance.Covariant => IntersectionType.construct(arguments)
              case Variance.Contravariant => SumType.construct(arguments)
              case Variance.Invariant => if (arguments.toSet.size == 1) arguments.head else return None
            }
          }
          supertypeSchema.instantiate(combinedArguments).asInstanceOf[Option[DeclaredType]]
      }
    }
  }

}
