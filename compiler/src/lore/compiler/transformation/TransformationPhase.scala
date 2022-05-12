package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{Definition, Registry}

object TransformationPhase {

  def process(definition: Definition)(implicit registry: Registry, reporter: Reporter): Unit = definition match {
    case definition: StructDefinition => StructTransformer.transform(definition)
    case variable: GlobalVariableDefinition => GlobalVariableTransformer.transform(variable)
    case mf: MultiFunctionDefinition => mf.functions.foreach(FunctionTransformer.transform)
    case spec: SpecDefinition => SpecTransformer.transform(spec)
    case _ =>
  }

}
