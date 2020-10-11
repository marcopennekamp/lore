package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Change the name to something more sensible (something not ambiguously confusing)... This is long overdue.
// TODO: How do we handle the naming of components which have type parameters? Still use the simple name
//       and check uniqueness irrespective of type arguments?
case class ComponentType(underlying: DeclaredType) extends Type {
  assert(underlying.isOwnable)

  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from declared type hashes.
    MurmurHash3.productHash(("component", underlying))
  }
}
