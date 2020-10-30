package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes immutable lists. Lists are covariant.
  */
case class ListType(element: Type) extends Type {
  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from type hashes.
    MurmurHash3.orderedHash(Vector(element), 0xfb04146c)
  }
}
