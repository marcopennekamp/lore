package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Rename all instances of "component" to "element".

// TODO: Strongly consider renaming product types to tuple types. This split between value and type naming is annoying.

case class ProductType(components: Vector[Type]) extends Type {
  override val hashCode: Int = MurmurHash3.unorderedHash(components)
}

object ProductType {
  val UnitType: ProductType = ProductType(Vector.empty)
}
