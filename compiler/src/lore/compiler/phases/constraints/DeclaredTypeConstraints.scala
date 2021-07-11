package lore.compiler.phases.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{DeclaredTypeDefinition, StructDefinition}

object DeclaredTypeConstraints {

  /**
    * Verifies:
    *   1. All struct constraints if the given definition is a struct.
    */
  def verify(definition: DeclaredTypeDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    definition match {
      case struct: StructDefinition => StructConstraints.verify(struct)
      case _ =>
    }
  }

}
