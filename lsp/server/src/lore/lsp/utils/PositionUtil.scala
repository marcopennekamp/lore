package lore.lsp.utils

import org.eclipse.lsp4j.{Location, Position, Range}

object PositionUtil {

  /**
    * Creates an LSP position from the start of the given Lore position.
    */
  def fromStartPosition(position: lore.compiler.core.Position): Position = {
    new Position(position.startLine - 1, position.startColumn - 1)
  }

  /**
    * Creates an LSP position from the end of the given Lore position.
    */
  def fromEndPosition(position: lore.compiler.core.Position): Position = {
    new Position(position.endLine - 1, position.endColumn - 1)
  }

  /**
    * Creates an LSP range from the given position.
    */
  def toRange(position: lore.compiler.core.Position): Range = {
    val start = fromStartPosition(position)
    val end = fromEndPosition(position)
    new Range(start, end)
  }

  /**
    * Creates an LSP location from the given position.
    */
  def toLocation(position: lore.compiler.core.Position): Location = {
    val uri = position.fragment.uri.getOrElse(
      throw new IllegalStateException(s"Cannot create an LSP Location from a position whose fragment doesn't have a path. Position: $position.")
    )
    new Location(uri, toRange(position))
  }

}
