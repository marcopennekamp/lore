package lore.compiler.phases

import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.{C, Compilation, Fragment}
import lore.parser.FragmentParser

class Phase1(sources: List[SourceFragment]) extends Phase[List[Fragment]] {
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
