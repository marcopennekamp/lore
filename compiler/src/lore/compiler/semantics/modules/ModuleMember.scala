package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.{BindingKind, NamePath}

/**
  * A member of a module, which represents either a type or a term based on the binding kind. Module members are owned
  * by global modules and referenced by local modules.
  *
  * Module member is a class instead of a case class because it's referenced by local modules. There should be exactly
  * one module member instance per actual module member.
  */
class ModuleMember(
  val namePath: NamePath,
  val bindingKind: BindingKind,
) {
  private var _positions: Vector[Position] = Vector.empty

  def addPosition(position: Position): Unit = {
    _positions :+= position
  }

  def positions: Vector[Position] = _positions
  def firstPosition: Position = _positions.headOption.getOrElse(Position.unknown)
}
