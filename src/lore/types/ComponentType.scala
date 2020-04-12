package lore.types

case class ComponentType(underlying: ClassType) extends Type {
  /**
    * A component type is abstract if its underlying class type is abstract. See the specification for an explanation.
    */
  override def isAbstract: Boolean = underlying.isAbstract

  override def toString: String = s"+$underlying"
}
