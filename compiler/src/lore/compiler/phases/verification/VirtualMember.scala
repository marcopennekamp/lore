package lore.compiler.phases.verification

import lore.definitions.MemberDefinition
import lore.types.Type

import scala.util.hashing.MurmurHash3

// TODO: How do we later compile virtual members? Is the name and type information enough?

/**
  * A member of a given type. This can either be a declared member or an implicitly accessible member.
  *
  * For example, a product type might define an implicit member for each component of the tuple. A list might have
  * a member such as 'size'.
  *
  * @param underlying The member definition underlying this virtual member.
  */
case class VirtualMember(
  name: String, tpe: Type, isComponent: Boolean = false, isMutable: Boolean = false,
  underlying: Option[MemberDefinition[Type]] = None,
) {
  override def equals(obj: Any): Boolean = obj match {
    case VirtualMember(name2, tpe2, _, _, _) => name == name2 && tpe == tpe2
    case _ => false
  }
  override def hashCode(): Int = MurmurHash3.productHash((name, tpe))
}
