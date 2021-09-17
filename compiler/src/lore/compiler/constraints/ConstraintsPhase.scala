package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.DeclaredSchemaDefinition

object ConstraintsPhase {

  def process()(implicit registry: Registry, reporter: Reporter): Unit = {
    CoreConstraints.verify()
  }

  def process(definition: DeclaredSchemaDefinition)(implicit registry: Registry, reporter: Reporter): Unit = DeclaredSchemaConstraints.verify(definition)

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = MultiFunctionConstraints.verify(mf)

}
