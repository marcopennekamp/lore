package lore.types

case class MapType(key: Type, value: Type) extends Type with OperatorType {
  /**
    * Maps are always concrete, since the empty map exists for all key/value types.
    */
  override def isAbstract = false

  override protected def precedence: TypePrecedence = TypePrecedence.Map
  override protected def operands: List[Type] = List(key, value)
  override protected def operator: String = "->"
}
