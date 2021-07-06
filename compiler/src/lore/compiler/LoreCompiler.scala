package lore.compiler

import lore.compiler.core.{Compilation, CompilerOptions, Fragment}
import lore.compiler.feedback.Feedback
import lore.compiler.phases.constraints.ConstraintsPhase
import lore.compiler.phases.generation.GenerationPhase
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.phases.resolution.ResolutionPhase
import lore.compiler.phases.transformation.TransformationPhase
import lore.compiler.phases.transpilation.TranspilationPhase
import lore.compiler.semantics.Registry
import lore.compiler.utils.Timer.timed

object LoreCompiler {

  /**
    * Compiles the given fragments, either resulting in a list of errors and warnings, or a completed compilation.
    */
  def compile(fragments: Vector[Fragment], options: CompilerOptions): Compilation[(Registry, String)] = {
    Feedback.loggerBlank.debug("")

    for {
      registry <- analyze(fragments, options)
      code <- generate(registry, options)
    } yield (registry, code)
  }

  /**
    * Analyzes a Lore program, which combines the parsing, resolution, constraints, and transformation phases.
    */
  def analyze(fragments: Vector[Fragment], options: CompilerOptions): Compilation[Registry] = {
    for {
      // Phase 1: Parse source files into a list of fragments.
      fragmentsWithDeclarations <- timed ("Parsing") (ParsingPhase.process (fragments) )
      // Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
      registry <- timed ("Resolution") (ResolutionPhase.process (fragmentsWithDeclarations) )
      // Phase 3: Check pre-transformation constraints.
      _ <- timed ("Constraints") (ConstraintsPhase.process (registry) )
      // Phase 4: Produce expression trees for functions and default property values. Resolves names in expressions and
      // infers local types.
      _ <- timed ("Transformation") (TransformationPhase.process (registry) )
    } yield registry
  }

  /**
    * Generates code for the Lore program specified by the Registry, combining the transpilation and generation phases.
    */
  def generate(registry: Registry, options: CompilerOptions): Compilation[String] = {
    for {
      // Phase 5: Transpile the Lore program to our target representation.
      target <- timed("Transpilation")(TranspilationPhase.process(options, registry))
      // Phase 6: Generate Javascript code from the target representation.
      code <- timed("Generation")(GenerationPhase.process(target))
    } yield code
  }

}
