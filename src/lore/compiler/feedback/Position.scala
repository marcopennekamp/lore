package lore.compiler.feedback

import lore.ast.Node.Index
import lore.compiler.Fragment

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
}
