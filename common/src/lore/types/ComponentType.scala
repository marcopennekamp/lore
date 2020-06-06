package lore.types

import scala.util.hashing.MurmurHash3

// TODO: How do we handle the naming of components which have type parameters? Still use the simple name
//       and check uniqueness irrespective of type arguments?
trait ComponentType extends Type {
  def underlying: ClassType

  /**
    * A component type is abstract if its underlying class type is abstract. See the specification for an explanation.
    */
  override def isAbstract: Boolean = underlying.isAbstract

  override val isPolymorphic = false

  override def string(precedence: TypePrecedence): String = s"+$underlying"
  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from class type hashes.
    MurmurHash3.productHash(("component", underlying))
  }
}
