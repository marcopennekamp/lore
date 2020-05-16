package lore.compiler.definitions

import lore.compiler.feedback.Position

trait PositionedDefinition {
  def position: Position
}
