package lore.compiler.types

import scala.util.hashing.MurmurHash3

case class TupleType(elements: Vector[Type]) extends Type {
  override val hashCode: Int = MurmurHash3.orderedHash(elements, 0x4baf1ec8)
}

object TupleType {
  val UnitType: TupleType = TupleType(Vector.empty)

  def apply(elements: Type*): TupleType = TupleType(elements.toVector)
}
