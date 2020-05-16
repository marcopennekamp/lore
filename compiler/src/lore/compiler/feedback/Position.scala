package lore.compiler.feedback

import lore.ast.Node.Index
import lore.compiler.Fragment

// TODO: Since we have to report errors at runtime with their positions in Lore code, we should implement some
//       kind of position source mapping for the runtime.

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, index: Index) {
  def <(position: Position): Boolean = {
    if (this.fragment != position.fragment) {
      this.fragment.name < position.fragment.name
    } else {
      this.index < position.index
    }
  }

  lazy val prettyIndex: String = fragment.input.prettyIndex(index)

  /**
    * The line number of the position as a 1-based index.
    */
  lazy val line: Int = {
    // Not the prettiest way to implement this, but fastparse doesn't seem to expose a line number interface.
    // This is the most convenient way to access line numbers, as far as I can see.
    Integer.parseInt(prettyIndex.split(":").head)
  }
}
