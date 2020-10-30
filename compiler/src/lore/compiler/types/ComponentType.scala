package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Change the name to something more sensible (something not ambiguously confusing)... This is long overdue.

case class ComponentType(underlying: DeclaredType) extends Type {
  assert(underlying.isOwnable)

  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from declared type hashes.
    MurmurHash3.productHash(("component", underlying))
  }
}
