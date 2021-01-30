package lore.compiler.phases.parsing

import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.syntax.DeclNode

object ParsingPhase {
  /**
    * Parses all fragments, resulting in declaration nodes or failing with parsing errors.
    */
  def process(sources: Vector[Fragment]): Compilation[Vector[DeclNode]] = {
    sources.map { source =>
      val parser = new FragmentParser()(source)
      parser.parsed
    }.simultaneous.map(_.flatten)
  }
}
