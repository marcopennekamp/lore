package lore.compiler.semantics.modules

import lore.compiler.semantics.definitions.BindingDefinition

/**
  * [[GlobalModuleMembers]] manages type or term global module members.
  */
class GlobalModuleMembers[A <: BindingDefinition](
  val globalModule: GlobalModule,
  val moduleMemberKind: ModuleMemberKind[A],
) extends ModuleMembers[A] {
  private var members: Map[String, A] = Map.empty

  /**
    * Returns all module members.
    */
  def all: Iterable[A] = members.values

  /**
    * Returns the module member called `memberName`, or `None` otherwise.
    */
  def get(memberName: String): Option[A] = members.get(memberName)

  /**
    * Whether this module has a member `memberName`.
    */
  def has(memberName: String): Boolean = members.contains(memberName)

  /**
    * Adds `moduleMember` to the members. Any constraints must be checked before using `add`, such as checking for
    * duplicates.
    */
  def add(moduleMember: A): Unit = members += moduleMember.simpleName -> moduleMember
}
