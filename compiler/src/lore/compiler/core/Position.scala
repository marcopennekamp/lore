package lore.compiler.core

import lore.compiler.syntax.Node.Index

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, startIndex: Index, endIndex: Index) extends Positioned {
  def <(other: Position): Boolean = {
    this.fragment.name < other.fragment.name || (this.fragment.name == other.fragment.name && this.startIndex < other.startIndex)
  }

  /**
    * The pretty index which is used for printing the position.
    *
    * This is a lazy value because it has heavy implications on parsing performance.
    */
  lazy val prettyIndex: String = fragment.input.prettyIndex(startIndex)

  /**
    * The line and column numbers of the position as 1-based indices.
    */
  lazy val (startLine, startColumn, endLine, endColumn): (Int, Int, Int, Int) = {
    // Not the prettiest way to implement this, but fastparse doesn't seem to expose a line/column interface. This is
    // the most convenient way to access line/column numbers, as far as I can see.
    val Array(startLine, startColumn) = prettyIndex.split(":").map(Integer.parseInt)
    val Array(endLine, endColumn) = fragment.input.prettyIndex(endIndex).split(":").map(Integer.parseInt)
    (startLine, startColumn, endLine, endColumn)
  }

  /**
    * The length of the position, possibly spanning multiple lines.
    */
  def length: Int = endIndex - startIndex

  /**
    * Creates a new position that spans from the start index of this position to the end index of `end`.
    */
  def to(end: Position): Position = {
    if (this.fragment != end.fragment) {
      throw CompilationException(s"Cannot create a spanning position from $this to $end. The positions must be located" +
        s" in the same fragment!")
    }
    Position(this.fragment, this.startIndex, end.endIndex)
  }

  /**
    * Creates a new 0-length position that refers to the end of this position.
    */
  def end: Position = Position(fragment, endIndex, endIndex)

  /**
    * The actual code from the start to the end of the position.
    */
  lazy val code: String = fragment.input.slice(startIndex, endIndex).strip()

  /**
    * The first line of `code`.
    */
  lazy val truncatedCode: String = code.takeWhile(_ != '\n')

  /**
    * A complete string representation of this position.
    */
  override def toString: String = s"${fragment.name} ($prettyIndex)"

  override def position: Position = this
}

object Position {
  /**
    * An "internal" position that signals compiler-generated code without any sensible anchor.
    */
  val internal: Position = Position(Fragment("internal (compiler-generated)", ""), 0, 0)

  /**
    * A position referring to an unknown fragment, for example a file that wasn't found.
    */
  val unknown: Position = Position(Fragment("unknown", ""), 0, 0)
}
