package lore.compiler

import lore.ast.Index

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
