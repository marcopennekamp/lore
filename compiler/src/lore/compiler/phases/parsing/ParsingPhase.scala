package lore.compiler.phases.parsing

import lore.compiler.ast.DeclNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.Fragment
import lore.compiler.phases.Phase

class ParsingPhase(sources: List[Fragment]) extends Phase[List[DeclNode]] {
  override lazy val result: C[List[DeclNode]] = parseAll()

  /**
    * Parses all fragments or fails with parsing errors.
    */
  def parseAll(): C[List[DeclNode]] = {
    sources.map { source =>
      val parser = new FragmentParser()(source)
      parser.parsed
    }.simultaneous.map(_.flatten)
  }
}
