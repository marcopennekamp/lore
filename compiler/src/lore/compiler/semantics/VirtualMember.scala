package lore.compiler.semantics

import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.types.Type

import scala.util.hashing.MurmurHash3

/**
  * A member of a given type. This can either be a declared member or an implicitly accessible member.
  *
  * @param underlying The member definition underlying this virtual member.
  *
  * TODO (shape): We can rename this to Member now that structs only contain properties.
  */
case class VirtualMember(
  name: String,
  tpe: Type,
  isMutable: Boolean = false,
  underlying: Option[StructPropertyDefinition] = None,
) {
  val isImmutable: Boolean = !isMutable

  override def equals(obj: Any): Boolean = obj match {
    case VirtualMember(name2, tpe2, _, _) => name == name2 && tpe == tpe2
    case _ => false
  }
  override def hashCode(): Int = MurmurHash3.productHash((name, tpe))
}
