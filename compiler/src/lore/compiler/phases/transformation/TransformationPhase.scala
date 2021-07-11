package lore.compiler.phases.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{DeclaredTypeDefinition, StructDefinition}

object TransformationPhase {

  def process(definition: DeclaredTypeDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    definition match {
      case definition: StructDefinition => StructTransformer.transform(definition)
      case _ =>
    }
  }

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    mf.functions.foreach(FunctionTransformer.transform)
  }

}
