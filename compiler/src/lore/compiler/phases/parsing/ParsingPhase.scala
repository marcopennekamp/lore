package lore.compiler.phases.parsing

import lore.compiler.syntax.DeclNode
import lore.compiler.core.{Compilation, Fragment, Phase}

class ParsingPhase(sources: List[Fragment]) extends Phase[List[DeclNode]] {
  override lazy val result: Compilation[List[DeclNode]] = parseAll()

  /**
    * Parses all fragments or fails with parsing errors.
    */
  def parseAll(): Compilation[List[DeclNode]] = {
    sources.map { source =>
      val parser = new FragmentParser()(source)
      parser.parsed
    }.simultaneous.map(_.flatten)
  }
}
