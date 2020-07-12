package lore.compiler.feedback

import lore.compiler.ast.Node.Index
import lore.compiler.core.Fragment

// TODO: Since we have to report errors at runtime with their positions in Lore code, we should implement some
//       kind of position source mapping for the runtime.

/**
 * A position identifies a code location across a whole Lore project.
 */
class Position(val fragmentName: String, val index: Index, val prettyIndex: String) {
  def <(position: Position): Boolean = {
    this.fragmentName < position.fragmentName || this.index < position.index
  }

  /**
   * The line number of the position as a 1-based index.
   */
  lazy val line: Int = {
    // Not the prettiest way to implement this, but fastparse doesn't seem to expose a line number interface.
    // This is the most convenient way to access line numbers, as far as I can see.
    Integer.parseInt(prettyIndex.split(":").head)
  }
}

class FragmentPosition(val fragment: Fragment, index: Index) extends Position(fragment.name, index, fragment.input.prettyIndex(index))
