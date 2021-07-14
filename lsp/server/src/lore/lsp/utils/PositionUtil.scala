package lore.lsp.utils

import org.eclipse.lsp4j.{Location, Position, Range}

object PositionUtil {

  /**
    * Creates a point LSP range from the given position.
    */
  def toRange(position: lore.compiler.core.Position): Range = {
    val start = new Position(position.startLine - 1, position.startColumn - 1)
    val end = new Position(position.endLine - 1, position.endColumn - 1)
    new Range(start, end)
  }

  /**
    * Creates a point LSP location from the given position.
    */
  def toLocation(position: lore.compiler.core.Position): Location = {
    val uri = position.fragment.uri.getOrElse(
      throw new IllegalStateException(s"Cannot create an LSP Location from a position whose fragment doesn't have a path. Position: $position.")
    )
    new Location(uri, toRange(position))
  }

}
