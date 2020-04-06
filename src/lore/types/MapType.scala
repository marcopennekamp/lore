package lore.types

case class MapType(key: Type, value: Type) extends Type {
  /**
    * Maps are always concrete, since the empty map exists for all key/value types.
    */
  override def isAbstract = false

  override def toString: String = s"$key -> $value"
}
