package lore.compiler.constraints

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{AliasDefinition, DeclaredSchemaDefinition}
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{Definition, Registry}

object ConstraintsPhase {

  def process(definition: Definition)(implicit registry: Registry, reporter: Reporter): Unit = definition match {
    case definition: DeclaredSchemaDefinition => DeclaredSchemaConstraints.verify(definition)
    case alias: AliasDefinition => AliasConstraints.verify(alias)
    case variable: GlobalVariableDefinition => GlobalVariableConstraints.verify(variable)
    case mf: MultiFunctionDefinition => MultiFunctionConstraints.verify(mf)
    case _ => throw CompilationException(s"Unexpected definition: $definition.")
  }

}
