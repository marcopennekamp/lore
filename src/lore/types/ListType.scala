package lore.types

import scala.util.hashing.MurmurHash3

case class ListType(element: Type) extends Type {
  /**
    * Lists are always concrete as the empty list exists for all element types.
    */
  override def isAbstract = false

  override def string(parentPrecedence: TypePrecedence): String = s"[${element.string(TypePrecedence.Parenthesized)}]"
  override val hashCode: Int = {
    // We use a product hash here to differentiate the hash code from type hashes.
    MurmurHash3.productHash(("list", element))
  }
}
