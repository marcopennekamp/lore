package lore.compiler

import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.phases.generation.GenerationPhase
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
      fragmentsWithDeclarations <- timed("Parsing", ParsingPhase.process(sources))
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- timed("Resolution", ResolutionPhase.process(fragmentsWithDeclarations))
      // Phase 3: Check constraints and ascribe types.
      _ <- timed("Transformation", TransformationPhase.process(registry))
      // Phase 4: Transpile the Lore program to our target representation.
      target <- timed("Transpilation", TranspilationPhase.process(options, registry))
      // Phase 5: Generate Javascript code from the target representation.
      code <- timed("Generation", GenerationPhase.process(target))
    } yield (registry, code)
  }

  def timed[R](phase: String, block: => R): R = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    println(s"$phase took: ${(end - start) / 1000000}ms")
    result
  }

}
