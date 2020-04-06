package lore.types

case class ComponentType(dataType: DataType) extends Type {
  /**
    * A component type is abstract if its underlying data type is abstract. See the specification for an explanation.
    */
  override def isAbstract: Boolean = dataType.isAbstract
}
