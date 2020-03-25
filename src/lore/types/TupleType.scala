package lore.types

case class TupleType(components: List[Type]) extends Type {
  /**
    * Since this is already a tuple, there is no need to enclose it in another tuple.
    */
  override def toTuple: TupleType = this

  /**
    * Whether the tuple type is abstract. A tuple type is abstract if one of its component types is abstract.
    */
  override def isAbstract: Boolean = components.exists(_.isAbstract)

  override def toString: String = "(" + components.mkString(", ") + ")"
}
