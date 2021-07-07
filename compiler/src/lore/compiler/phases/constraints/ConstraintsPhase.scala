package lore.compiler.phases.constraints

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.DeclaredTypeDefinition

object ConstraintsPhase {

  def process(definition: DeclaredTypeDefinition)(implicit registry: Registry): Verification = DeclaredTypeConstraints.verify(definition)

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry): Verification = MultiFunctionConstraints.verify(mf)

}
