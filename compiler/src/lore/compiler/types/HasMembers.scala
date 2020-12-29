package lore.compiler.types

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.members.{Member, MemberExplorer, MemberMap}
import lore.compiler.types.HasMembers.MemberNotFound

/**
  * An aspect of a type that concerns itself with the members which can be accessed through a value of the type.
  */
trait HasMembers { self: Type =>
  private var _members: Option[MemberMap] = None

  /**
    * The map of all members that can be accessed through a value of this type. The map is computed once by the
    * [[MemberExplorer]] and then cached by this type.
    */
  def members(implicit registry: Registry): MemberMap = this.synchronized {
    _members.getOrElse {
      val members = MemberExplorer.members(self)
      _members = Some(members)
      members
    }
  }

  /**
    * Finds a member with the given name within the member map of this type.
    */
  def member(name: String)(implicit accessPosition: Position, registry: Registry): Compilation[Member] = {
    members.get(name) match {
      case None => Compilation.fail(MemberNotFound(name, this, accessPosition))
      case Some(member) => member.compiled
    }
  }
}

object HasMembers {
  case class MemberNotFound(name: String, tpe: Type, pos: Position) extends Error(pos) {
    override def message: String = s"A member $name does not exist within the type $tpe."
  }
}
