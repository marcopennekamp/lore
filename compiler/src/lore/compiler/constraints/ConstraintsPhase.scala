package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{AliasDefinition, DeclaredSchemaDefinition}

object ConstraintsPhase {

  def process(definition: DeclaredSchemaDefinition)(implicit registry: Registry, reporter: Reporter): Unit = DeclaredSchemaConstraints.verify(definition)

  def process(alias: AliasDefinition)(implicit registry: Registry, reporter: Reporter): Unit = AliasConstraints.verify(alias)

  def process(mf: MultiFunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = MultiFunctionConstraints.verify(mf)

}
