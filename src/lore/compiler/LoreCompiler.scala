package lore.compiler

import lore.compiler.Compilation.C
import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.phases.verification.VerificationPhase
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.phases.resolution.ResolutionPhase

/**
  * The compiler instance orchestrates compilation through all phases.
  */
class LoreCompiler(val sources: List[SourceFragment]) {
  /**
    * Compiles the given sources, either resulting in a list of errors and warnings or a completed compilation.
    */
  def compile(): C[Registry] = { // TODO: Registry isn't the actual return type. We need to figure that out later.
    for {
      // Phase 1: Parse source files into a list of fragments.
      fragments <- new ParsingPhase(sources).result
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- new ResolutionPhase(fragments).result
      // Phase 3: Check constraints and ascribe types.
      _ <- new VerificationPhase()(registry).result
    } yield registry
  }
}

object LoreCompiler {
  case class SourceFragment(name: String, code: String)
}
