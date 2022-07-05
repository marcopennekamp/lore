package lore.compiler.semantics

import lore.compiler.core.Position

// TODO (multi-import): Is this only used once in GlobalModule?

/**
  * [[PositionsProperty]] allows adding positions to an object and also provides various getters.
  */
trait PositionsProperty {
  private var _positions: Vector[Position] = Vector.empty

  def addPosition(position: Position): Unit = _positions :+= position

  def positions: Vector[Position] = _positions
  def position: Position = positions.headOption.getOrElse(Position.unknown)
}
