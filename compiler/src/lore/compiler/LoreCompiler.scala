package lore.compiler

import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.phases.resolution.ResolutionPhase
import lore.compiler.phases.transpilation.TranspilationPhase
import lore.compiler.phases.transformation.TransformationPhase
import lore.compiler.semantics.Registry

/**
  * The compiler instance orchestrates compilation through all phases.
  */
class LoreCompiler(val sources: Vector[Fragment], val options: CompilerOptions) {
  /**
    * Compiles the given sources, either resulting in a list of errors and warnings or a completed compilation.
    */
  def compile(): Compilation[(Registry, String)] = {
    implicit val options: CompilerOptions = this.options
    for {
      // Phase 1: Parse source files into a list of fragments.
      fragmentsWithDeclarations <- timedPhase("Parsing", new ParsingPhase(sources).result)
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- timedPhase("Resolution", new ResolutionPhase(fragmentsWithDeclarations).result)
      // Phase 3: Check constraints and ascribe types.
      _ <- timedPhase("Transformation", new TransformationPhase()(registry).result)
      // Phase 4: Transpile the Lore program to Javascript.
      output <- timedPhase("Transpilation", new TranspilationPhase()(options, registry).result)
    } yield (registry, output)
  }

  def timedPhase[R](phase: String, block: => R): R = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    println(s"$phase took: ${(end - start) / 1000000}ms")
    result
  }
}
