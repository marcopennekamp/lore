package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.VirtualMember
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.types.{ComponentType, DeclaredType, StructType, Type}

// TODO: "mutable" should actually be "writeable", since immutability implies that the whole data structure within
//       that member is unchangeable, while we are actually just gating the top-level write access to the member.
//       This is a subtle difference, but it should be honored. Perhaps we can later introduce a "deep" kind of
//       immutability which doesn't just make a member readonly, but actually applies to the whole data structure.

/**
  * A data type member, that is, either a property or a component.
  */
sealed trait MemberDefinition extends Positioned {
  def name: String
  def tpe: Type
  def isMutable: Boolean = false
  def isComponent: Boolean = false
  def asParameter: ParameterDefinition = new ParameterDefinition(name, tpe, position)
  def asVirtualMember: VirtualMember = VirtualMember(name, tpe, isComponent = isComponent, isMutable = isMutable, underlying = Some(this))
}

class PropertyDefinition(
  override val name: String,
  override val tpe: Type,
  override val isMutable: Boolean,
  override val position: Position,
) extends MemberDefinition

/**
  * @param name The name of the component (and at the same time its type name).
  */
class ComponentDefinition(
  override val name: String,
  override val tpe: DeclaredType,
  override val position: Position,
) extends MemberDefinition {
  override def isComponent: Boolean = true
  lazy val componentType: ComponentType = ComponentType(tpe)
}
