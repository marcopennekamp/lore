package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes maps. Maps are invariant.
  */
case class MapType(key: Type, value: Type) extends Type with OperatorType {
  override protected def precedence: TypePrecedence = TypePrecedence.Map
  override protected def operands: List[Type] = List(key, value)
  override protected def operator: String = "->"

  override val hashCode: Int = MurmurHash3.productHash((key, value))
}
