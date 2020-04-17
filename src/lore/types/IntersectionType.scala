package lore.types

case class IntersectionType private (types: Set[Type]) extends Type with OperatorType {
  // An intersection type must not be empty!
  assert(types.nonEmpty)

  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  def isAnyComponentSubtypeOf(candidateSupertype: Type): Boolean = {
    types.exists(t => Subtyping.isSubtype(t, candidateSupertype))
  }

  /**
    * An intersection type is abstract if any of its component types are abstract.
    *
    * The reasoning is that the value inhabiting the intersection type will need to have each component type as its
    * type. So there can't be a value that has a type as its exact type (not a subtype) that is abstract. Hence, any
    * one abstract component can turn an intersection type abstract.
    *
    * Note that we consider the idea of augmenting label types here. If the intersection type contains at least one
    * non-label type, we ignore label types in the consideration.
    */
  override def isAbstract: Boolean = {
    val exceptLabels = types.filter(!_.isInstanceOf[LabelType])

    // If the intersection type consists only of labels, it is NOT an augmented type and thus abstract since
    // non-augmenting label types are abstract.
    if (exceptLabels.isEmpty) {
      return true
    }

    // In all other cases, we decide abstractness WITHOUT taking augmenting labels into account.
    exceptLabels.exists(_.isAbstract)
  }

  override protected def precedence: TypePrecedence = TypePrecedence.Intersection
  override protected def operands: List[Type] = types.toList
  override protected def operator: String = "&"
}

object IntersectionType {
  /**
    * Constructs type from the given types and flattens it if necessary. If the resulting intersection type
    * has only one component, this type is returned instead.
    */
  def construct(types: Set[Type]): Type = {
    val intersection = new IntersectionType(types.flatMap {
      // If the directly nested type is an intersection type, flatten it.
      case t: IntersectionType => t.types
      case t => Set(t)
    })
    if (intersection.types.size == 1) intersection.types.head else intersection
  }

  def construct(types: List[Type]): Type = construct(types.toSet)
}
