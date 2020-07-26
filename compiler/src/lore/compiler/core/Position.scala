package lore.compiler.core

import lore.compiler.syntax.Node.Index

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, index: Index) {
  def <(position: Position): Boolean = {
    this.fragment.name < position.fragment.name || this.index < position.index
  }

  /**
    * The pretty index which is used for printing the position.
    */
  val prettyIndex: String = fragment.input.prettyIndex(index)

  /**
    * The line number of the position as a 1-based index.
    */
  lazy val line: Int = {
    // Not the prettiest way to implement this, but fastparse doesn't seem to expose a line number interface.
    // This is the most convenient way to access line numbers, as far as I can see.
    Integer.parseInt(prettyIndex.split(":").head)
  }

  /**
    * A complete string representation of this position.
    */
  override def toString: String = s"${fragment.name} ($prettyIndex)"
}
