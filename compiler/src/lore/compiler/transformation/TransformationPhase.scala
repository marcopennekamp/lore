package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{DeclaredSchemaDefinition, StructDefinition}
import lore.compiler.semantics.variables.GlobalVariableDefinition

object TransformationPhase {

  def process(definition: DeclaredSchemaDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    definition match {
      case definition: StructDefinition => StructTransformer.transform(definition)
      case _ =>
    }
  }

  def process(variable: GlobalVariableDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    GlobalVariableTransformer.transform(variable)
  }

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    mf.functions.foreach(FunctionTransformer.transform)
  }

}
