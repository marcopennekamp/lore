package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: How do we handle the naming of components which have type parameters? Still use the simple name
//       and check uniqueness irrespective of type arguments?
case class ComponentType(underlying: ClassType) extends Type {
  override def string(precedence: TypePrecedence): String = s"+$underlying"
  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from class type hashes.
    MurmurHash3.productHash(("component", underlying))
  }
}
