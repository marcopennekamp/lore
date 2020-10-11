package lore.compiler.phases.parsing

import lore.compiler.syntax.DeclNode
import lore.compiler.core.{Compilation, Fragment, Phase}

class ParsingPhase(sources: Vector[Fragment]) extends Phase[Vector[DeclNode]] {
  override lazy val result: Compilation[Vector[DeclNode]] = parseAll()

  /**
    * Parses all fragments or fails with parsing errors.
    */
  def parseAll(): Compilation[Vector[DeclNode]] = {
    sources.map { source =>
      val parser = new FragmentParser()(source)
      parser.parsed
    }.simultaneous.map(_.flatten)
  }
}
