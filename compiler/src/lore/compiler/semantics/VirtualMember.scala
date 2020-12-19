package lore.compiler.semantics

import lore.compiler.semantics.structures.PropertyDefinition
import lore.compiler.types.Type

import scala.util.hashing.MurmurHash3

/**
  * A member of a given type. This can either be a declared member or an implicitly accessible member.
  *
  * If a virtual member is a component, we will have to resolve it at run-time. In the case of being part of a
  * component type +Animal, the member might be named and typed Animal, while the actual member of the given
  * entity object (at run-time at least) might be named and typed Fox. In the case of entity types, we might have
  * an entity E1 with a component Immunity which is overridden by entity E2 and the component PoisonImmunity.
  *
  * @param underlying The member definition underlying this virtual member.
  *
  * TODO (shape): We can rename this to Member now that structs only contain properties.
  */
case class VirtualMember(
  name: String,
  tpe: Type,
  isComponent: Boolean = false,
  isMutable: Boolean = false,
  underlying: Option[PropertyDefinition] = None,
) {
  val isImmutable: Boolean = !isMutable

  override def equals(obj: Any): Boolean = obj match {
    case VirtualMember(name2, tpe2, _, _, _) => name == name2 && tpe == tpe2
    case _ => false
  }
  override def hashCode(): Int = MurmurHash3.productHash((name, tpe))
}
