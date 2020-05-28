package lore.compiler

import lore.compiler.core.Compilation.C
import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.core.Registry
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.phases.resolution.ResolutionPhase
import lore.compiler.phases.transpilation.TranspilationPhase
import lore.compiler.phases.verification.VerificationPhase

/**
  * The compiler instance orchestrates compilation through all phases.
  */
class LoreCompiler(val sources: List[SourceFragment]) {
  /**
    * Compiles the given sources, either resulting in a list of errors and warnings or a completed compilation.
    */
  def compile(): C[(Registry, String)] = {
    for {
      // Phase 1: Parse source files into a list of fragments.
      fragments <- new ParsingPhase(sources).result
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- new ResolutionPhase(fragments).result
      // Phase 3: Check constraints and ascribe types.
      _ <- new VerificationPhase()(registry).result
      // Phase 4: Transpile the Lore program to Javascript.
      output <- new TranspilationPhase()(registry).result
    } yield (registry, output)
  }
}

object LoreCompiler {
  case class SourceFragment(name: String, code: String)
}
