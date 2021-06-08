package lore.compiler.core

import lore.compiler.syntax.Node.Index

/**
  * A position identifies a code location across a whole Lore project.
  */
case class Position(fragment: Fragment, index: Index) {
  def <(other: Position): Boolean = {
    this.fragment.name < other.fragment.name || (this.fragment.name == other.fragment.name && this.index < other.index)
  }

  /**
    * The pretty index which is used for printing the position.
    *
    * This is a lazy value because it has heavy implications on parsing performance.
    */
  lazy val prettyIndex: String = fragment.input.prettyIndex(index)

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
  val wildcard: Position = Position(Fragment("wildcard", ""), 0)

  /**
    * An "internal" position that signals compiler-generated code without any sensible anchor.
    */
  val internal: Position = Position(Fragment("internal (compiler-generated)", ""), 0)

  /**
    * A position referring to an unknown fragment, for example a file that wasn't found.
    */
  val unknown: Position = Position(Fragment("unknown", ""), 0)
}
