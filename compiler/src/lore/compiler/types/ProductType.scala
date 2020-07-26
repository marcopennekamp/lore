package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Rename all instances of "component" to "element".

case class ProductType(components: List[Type]) extends Type {
  /**
    * Since this is already a tuple, there is no need to enclose it in another tuple.
    */
  override def toTuple: ProductType = this

  override val hashCode: Int = MurmurHash3.unorderedHash(components)
}

object ProductType {
  val UnitType: ProductType = ProductType(List.empty)
}
