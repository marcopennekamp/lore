package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Change the name to something more sensible (something not ambiguously confusing)... This is long overdue.

case class ComponentType(underlying: DeclaredType) extends Type {
  assert(underlying.isOwnable)

  override val hashCode: Int = {
    MurmurHash3.orderedHash(Vector(underlying), 0x4cab1ec0)
  }
}
