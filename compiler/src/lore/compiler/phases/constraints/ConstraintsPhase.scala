package lore.compiler.phases.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.DeclaredSchemaDefinition

object ConstraintsPhase {

  def process(definition: DeclaredSchemaDefinition)(implicit registry: Registry, reporter: Reporter): Unit = DeclaredTypeConstraints.verify(definition)

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = MultiFunctionConstraints.verify(mf)

}
