package lore.lsp.utils

import org.eclipse.lsp4j.{Location, Position, Range}

object PositionUtil {

  /**
    * Creates a one-line LSP range from the given starting position and the desired length of the range.
    */
  def toRange(position: lore.compiler.core.Position, length: Int): Range = {
    val start = new Position(position.line - 1, position.column - 1)
    val end = new Position(start.getLine, start.getCharacter + length)
    new Range(start, end)
  }

  /**
    * Creates a point LSP range from the given starting position.
    */
  def toRange(position: lore.compiler.core.Position): Range = toRange(position, 0)

  /**
    * Creates a one-line LSP location from the given starting position and the desired length of the range.
    */
  def toLocation(position: lore.compiler.core.Position, length: Int): Location = {
    val uri = position.fragment.uri.getOrElse(
      throw new IllegalStateException(s"Cannot create an LSP Location from a position whose fragment doesn't have a path. Position: $position.")
    )
    new Location(uri, toRange(position, length))
  }

  /**
    * Creates a point LSP location from the given starting position.
    */
  def toLocation(position: lore.compiler.core.Position): Location = toLocation(position, 0)

}
