package lore.compiler

import lore.compiler.core.Compilation.{CompilationVectorExtension, EmptyFailure, Verification}
import lore.compiler.core.{Compilation, CompilerOptions, Fragment}
import lore.compiler.feedback.Feedback
import lore.compiler.phases.constraints.ConstraintsPhase
import lore.compiler.phases.generation.GenerationPhase
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.phases.resolution.ResolutionPhase
import lore.compiler.phases.transformation.TransformationPhase
import lore.compiler.phases.transpilation.TranspilationPhase
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.types.DeclaredType
import lore.compiler.utils.CollectionExtensions.VectorExtension
import lore.compiler.utils.Timer.timed

object LoreCompiler {

  /**
    * Compiles the given fragments, either resulting in a list of errors and warnings, or a completed compilation.
    */
  def compile(fragments: Vector[Fragment], options: CompilerOptions): Compilation[(Registry, String)] = {
    Feedback.loggerBlank.debug("")

    // Only continue the compilation if analysis is an explicit success. We don't want to generate code from a
    // program that had compilation errors.
    val compilation = analyze(fragments)
    if (compilation.isSuccess) {
      compilation.map { registry =>
        val code = generate(registry, options)
        (registry, code)
      }
    } else EmptyFailure(compilation.feedback)
  }

  /**
    * Analyzes a Lore program, which combines the parsing, resolution, constraints, and transformation phases:
    *
    *   - Phase 1: Parse source files into a list of fragments.
    *   - Phase 2: Resolve declarations using DeclarationResolver and build the Registry.
    *   - Phase 3: Check pre-transformation constraints.
    *   - Phase 4: Produce expression trees for functions and default property values. Resolves names in expressions and
    *              infers local types.
    *
    * Phases 3 and 4 are handled simultaneously for each individual type declaration and multi-function. If there is an
    * error in phase 3, phase 4 won't be invoked for that particular entity. This leads to more fine-grained errors for
    * each individual entity.
    */
  def analyze(fragments: Vector[Fragment]): Compilation[Registry] = {
    for {
      fragmentsWithDeclarations <- timed("Parsing")(ParsingPhase.process(fragments))
      registry <- timed("Resolution")(ResolutionPhase.process(fragmentsWithDeclarations))
      _ <- timed("Constraints & Transformation")(analyze(registry))
    } yield registry
  }

  private def analyze(implicit registry: Registry): Verification = {
    val declaredTypeDefinitions = registry.typesInOrder.map(_._2).filterType[DeclaredType].map(_.definition)
    val multiFunctions = registry.multiFunctions.values.toVector

    (
      declaredTypeDefinitions.map(analyze).simultaneous,
      multiFunctions.map(analyze).simultaneous,
    ).simultaneous.verification
  }

  private def analyze(definition: DeclaredTypeDefinition)(implicit registry: Registry): Verification = {
    ConstraintsPhase.process(definition).flatMapSuccess(_ => TransformationPhase.process(definition))
  }

  private def analyze(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    ConstraintsPhase.process(mf).flatMapSuccess(_ => TransformationPhase.process(mf))
  }

  /**
    * Generates code for the Lore program specified by the Registry, combining the transpilation and generation phases:
    *
    *   - Phase 5: Transpile the Lore program to our target representation.
    *   - Phase 6: Generate Javascript code from the target representation.
    */
  def generate(registry: Registry, options: CompilerOptions): String = {
    val target = timed("Transpilation")(TranspilationPhase.process(options, registry))
    timed("Generation")(GenerationPhase.process(target))
  }

}
