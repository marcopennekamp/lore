package lore.compiler.phases.parsing

import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.syntax.DeclNode

object ParsingPhase {
  /**
    * Parses all fragments, resulting in declaration nodes or failing with parsing errors.
    */
  def process(fragments: Vector[Fragment]): Compilation[Vector[DeclNode]] = {
    fragments.map { fragment =>
      val parser = new FragmentParser()(fragment)
      parser.parsed
    }.simultaneous.map(_.flatten)
  }
}
