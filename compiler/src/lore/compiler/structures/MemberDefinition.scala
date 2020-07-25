package lore.compiler.structures

import lore.compiler.core.Compilation.C
import lore.compiler.core.feedback.{Position, Positioned}
import lore.compiler.functions.ParameterDefinition
import lore.compiler.phases.verification.VirtualMember
import lore.compiler.types.{ClassType, ComponentType, Type, TypingDeferred}

// TODO: "mutable" should actually be "writeable", since immutability implies that the whole data structure within
//       that member is unchangeable, while we are actually just gating the top-level write access to the member.
//       This is a subtle difference, but it should be honored. Perhaps we can later introduce a "deep" kind of
//       immutability which doesn't just make a member readonly, but actually applies to the whole data structure.

/**
  * A data type member, that is, either a property or a component.
  */
sealed trait MemberDefinition[+T <: Type] extends Positioned with TypingDeferred[T] {
  def name: String
  def isMutable: Boolean = false
  def isComponent: Boolean = false
  def asParameter: ParameterDefinition = new ParameterDefinition(name, typeResolver, position)
  def asVirtualMember: VirtualMember = VirtualMember(name, tpe, isComponent = isComponent, isMutable = isMutable, underlying = Some(this))
}

class PropertyDefinition(
  override val name: String, override val typeResolver: () => C[Type], override val isMutable: Boolean,
  override val position: Position,
) extends MemberDefinition[Type]

/**
  * @param name The name of the component (and at the same time its type name).
  * @param overrides The component name of the superclass that this component overrides.
  */
class ComponentDefinition(
  override val name: String, override val typeResolver: () => C[ClassType], val overrides: Option[String],
  override val position: Position,
) extends MemberDefinition[ClassType] {
  override def isComponent: Boolean = true
  lazy val componentType: ComponentType = ComponentType(tpe)
}
