package lore.definitions

import lore.compiler.feedback.Position

trait PositionedDefinition {
  def position: Position
}
