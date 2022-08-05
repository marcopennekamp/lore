package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.Registry
import lore.compiler.semantics.definitions.Definition
import lore.compiler.types.StructSchema

// TODO (multi-import): Find a better name for this phase.

object TransformationPhase {

  def process(definition: Definition)(implicit registry: Registry, reporter: Reporter): Unit = definition match {
    case struct: StructSchema => StructTransformer.transform(struct)
    case variable: GlobalVariableDefinition => GlobalVariableTransformer.transform(variable)
    case mf: MultiFunctionDefinition => mf.functions.foreach(FunctionTransformer.transform)
    case spec: SpecDefinition => SpecTransformer.transform(spec)
    case _ =>
  }

}
