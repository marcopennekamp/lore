package lore.compiler

import lore.compiler.constraints.ConstraintsPhase
import lore.compiler.core.{CompilerOptions, Fragment}
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.generation.GenerationPhase
import lore.compiler.parsing.ParsingPhase
import lore.compiler.resolution.ResolutionPhase
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.DeclaredSchemaDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.transformation.TransformationPhase
import lore.compiler.transpilation.TranspilationPhase
import lore.compiler.types.DeclaredSchema
import lore.compiler.utils.CollectionExtensions.VectorExtension
import lore.compiler.utils.Timer.timed

object LoreCompiler {

  /**
    * Compiles the given fragments, resulting in a registry, the generated code if no errors have been found, and a
    * list of feedback.
    */
  def compile(fragments: Vector[Fragment], options: CompilerOptions)(implicit reporter: Reporter): (Option[Registry], Option[String]) = {
    Feedback.loggerBlank.debug("")

    // Only continue the compilation if analysis is an explicit success. We don't want to generate code from an
    // erroneous program.
    val registry = analyze(fragments, exitEarly = true)
    val code = registry.flatMap {
      registry => if (!reporter.hasErrors) Some(generate(registry, options)) else None
    }

    (registry, code)
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
    *
    * Compilation will only terminate in phase 1 or 2 if `exitEarly` is true. We can always try to produce a Registry
    * from a subset of the program. This is beneficial for IDE support, but not much use if the compiler is run from
    * the command line. In the latter case, a parsing error that renders a whole fragment unusable would be buried
    * beneath a mountain of other errors.
    */
  def analyze(fragments: Vector[Fragment], exitEarly: Boolean)(implicit reporter: Reporter): Option[Registry] = {
    val fragmentModules = timed("Parsing")(ParsingPhase.process(fragments))
    if (exitEarly && reporter.hasErrors) return None

    val registry = timed("Resolution")(ResolutionPhase.process(fragmentModules))
    if (exitEarly && reporter.hasErrors) return Some(registry)

    timed("Constraints & Transformation")(analyze(registry, reporter))
    Some(registry)
  }

  private def analyze(implicit registry: Registry, reporter: Reporter): Unit = {
    val declaredSchemaDefinitions = registry.schemasInOrder.map(_._2).filterType[DeclaredSchema].map(_.definition)
    val globalVariables = registry.globalVariables.values.toVector
    val multiFunctions = registry.multiFunctions.values.toVector

    declaredSchemaDefinitions.foreach(analyze(_, reporter))
    globalVariables.foreach(analyze(_, reporter))
    multiFunctions.foreach(analyze(_, reporter))
  }

  private def analyze(definition: DeclaredSchemaDefinition, parentReporter: Reporter)(implicit registry: Registry): Unit = {
    MemoReporter.chain(parentReporter)(
      implicit reporter => ConstraintsPhase.process(definition),
      implicit reporter => TransformationPhase.process(definition),
    )
  }

  private def analyze(definition: GlobalVariableDefinition, parentReporter: Reporter)(implicit registry: Registry): Unit = {
    MemoReporter.chain(parentReporter)(
      implicit reporter => TransformationPhase.process(definition),
    )
  }

  private def analyze(mf: MultiFunctionDefinition, parentReporter: Reporter)(implicit registry: Registry): Unit = {
    MemoReporter.chain(parentReporter)(
      implicit reporter => ConstraintsPhase.process(mf),
      implicit reporter => TransformationPhase.process(mf),
    )
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
