package lore.compiler

import lore.compiler.LoreCompiler.SourceFragment
import lore.compiler.phases.Phase1

/**
  * The compiler instance orchestrates compilation through all phases.
  */
class LoreCompiler(val sources: List[SourceFragment]) {
  private var fragments: List[Fragment] = List.empty

  def compile(): Unit = {
    // Phase 1: Parse source files into a list of fragments.
    val phase1 = new Phase1(sources)
    fragments = phase1.result match {
      case Errors(_, _) =>
        println("Parsing failed with errors. Aborting compilation...")
        return
      case Result(fragments, _) =>
        println("Phase 1: Parsing was successful.")
        fragments
    }

    // TODO: Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
    val declarationResolver = new DeclarationResolver
    println(fragments.map(declarationResolver.addFragment).simultaneous.map(_ => ()))
    println(declarationResolver.buildRegistry())
  }
}

object LoreCompiler {
  case class SourceFragment(name: String, code: String)
}
