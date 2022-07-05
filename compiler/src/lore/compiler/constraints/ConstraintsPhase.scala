package lore.compiler.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.definitions.Definition
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.{AliasSchema, DeclaredSchema}

object ConstraintsPhase {

  def process(definition: Definition)(implicit registry: Registry, reporter: Reporter): Unit = definition match {
    case alias: AliasSchema => AliasConstraints.verify(alias)
    case schema: DeclaredSchema => DeclaredSchemaConstraints.verify(schema)
    case variable: GlobalVariableDefinition => GlobalVariableConstraints.verify(variable)
    case mf: MultiFunctionDefinition => MultiFunctionConstraints.verify(mf)
    case spec: SpecDefinition => SpecConstraints.verify(spec)
    case _ =>
  }

}
