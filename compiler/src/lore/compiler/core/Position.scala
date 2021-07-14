package lore.compiler.core

import lore.compiler.syntax.Node.Index

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, startIndex: Index, endIndex: Index) {
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
    * A complete string representation of this position.
    */
  override def toString: String = s"${fragment.name} ($prettyIndex)"

  /**
    * We have to override the equals method to incorporate the notion of a wildcard position, which is used in
    * tests. A wildcard position is always equal to any other position, including itself.
    */
  override def equals(obj: Any): Boolean = obj match {
    case other: Position => this.eq(Position.wildcard) || other.eq(Position.wildcard) || super.equals(other)
    case _ => false
  }
}

object Position {
  /**
    * A Position that is equal to any other position. This is used in tests to make nodes equal regardless of their
    * position. It should never be used by the compiler!
    */
  val wildcard: Position = Position(Fragment("wildcard", ""), 0, 0)

  /**
    * An "internal" position that signals compiler-generated code without any sensible anchor.
    */
  val internal: Position = Position(Fragment("internal (compiler-generated)", ""), 0, 0)

  /**
    * A position referring to an unknown fragment, for example a file that wasn't found.
    */
  val unknown: Position = Position(Fragment("unknown", ""), 0, 0)
}
