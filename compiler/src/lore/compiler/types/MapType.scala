package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes maps. Maps are invariant.
  */
case class MapType(key: Type, value: Type) extends Type {
  override val hashCode: Int = MurmurHash3.productHash((key, value))
}
