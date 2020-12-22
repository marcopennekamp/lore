package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Strongly consider renaming product types to tuple types. This split between value and type naming is annoying.

case class ProductType(elements: Vector[Type]) extends Type {
  override val hashCode: Int = MurmurHash3.orderedHash(elements, 0x4baf1ec8)
}

object ProductType {
  val UnitType: ProductType = ProductType(Vector.empty)
}
