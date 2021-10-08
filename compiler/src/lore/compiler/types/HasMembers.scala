package lore.compiler.types

import lore.compiler.semantics.members.{Member, MemberExplorer, MemberMap}

/**
  * An aspect of a type that concerns itself with the members which can be accessed through a value of the type.
  */
trait HasMembers { self: Type =>
  /**
    * The map of all members that can be accessed through a value of this type. The map is computed once by the
    * [[MemberExplorer]] and then cached by this type.
    */
  lazy val members: MemberMap = MemberExplorer.members(self)

  /**
    * Finds a member with the given name within the member map of this type. Does not report an error if the member
    * cannot be found.
    */
  def member(name: String): Option[Member] = members.get(name)
}
