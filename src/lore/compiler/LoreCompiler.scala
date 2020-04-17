package lore.compiler

import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.phases.{Phase1, Phase2}

/**
  * The compiler instance orchestrates compilation through all phases.
  */
class LoreCompiler(val sources: List[SourceFragment]) {
  /**
    * Compiles the given sources, either resulting in a list of errors and warnings or a completed compilation.
    */
  def compile(): C[Registry] = { // TODO: Unit isn't the actual return type. We need to figure that out later.
    for {
      // Phase 1: Parse source files into a list of fragments.
      fragments <- new Phase1(sources).result
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- new Phase2(fragments).result
    } yield registry
  }
}

object LoreCompiler {
  case class SourceFragment(name: String, code: String)
}
