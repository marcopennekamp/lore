package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{DeclaredTypeDefinition, StructDefinition}

object TransformationPhase {

  def process(definition: DeclaredTypeDefinition)(implicit registry: Registry): Verification = {
    definition match {
      case definition: StructDefinition => StructTransformer.transform(definition)
      case _ => Verification.succeed
    }
  }

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = {
    mf.functions.map(FunctionTransformer.transform).simultaneous.verification
  }

}
