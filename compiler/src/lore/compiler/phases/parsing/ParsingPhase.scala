package lore.compiler.phases.parsing

import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.feedback.{Error, Position}
import lore.compiler.phases.Phase

class ParsingPhase(sources: List[SourceFragment]) extends Phase[List[Fragment]] {
  override lazy val result: C[List[Fragment]] = parseAll()

  case class ParsingError(fastparseError: String, pos: Position) extends Error(pos) {
    override def message: String = s"The file had parsing errors: $fastparseError"
  }

  /**
    * Parses all fragments or fails with parse errors.
    */
  def parseAll(): C[List[Fragment]] = {
    sources.map { source =>
      FragmentParser.parse(source) match {
        case Left(errorMessage) => Compilation.fail(ParsingError(errorMessage, new Position(source.name, 0, "1:0")))
        case Right(fragment) => Compilation.succeed(fragment)
      }
    }.simultaneous
  }
}
