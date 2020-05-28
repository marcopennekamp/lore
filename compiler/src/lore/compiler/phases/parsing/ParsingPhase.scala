package lore.compiler.phases.parsing

import lore.compiler.core.Compilation.C
import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.phases.Phase
import lore.compiler.core.{Compilation, Fragment}

class ParsingPhase(sources: List[SourceFragment]) extends Phase[List[Fragment]] {
  override lazy val result: C[List[Fragment]] = parseAll()

  /**
    * Parses all fragments or fails with parse errors.
    */
  def parseAll(): C[List[Fragment]] = {
    sources.map { source =>
      FragmentParser.parse(source) match {
        // TODO: Would be great to turn these into actual compilation errors instead of doing a println. But an Error
        //       currently expects a Fragment object for its Position, so we can't construct an Error without having
        //       a Fragment first.
        case Left(errorMessage) => println(s"${source.name}: $errorMessage"); Compilation.fail()
        case Right(fragment) => Compilation.succeed(fragment)
      }
    }.simultaneous
  }
}
