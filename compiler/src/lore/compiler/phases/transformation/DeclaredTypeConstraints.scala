package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{DeclaredTypeDefinition, StructDefinition}

object DeclaredTypeConstraints {

  /**
    * Verifies:
    *   1. All struct constraints if the given definition is a struct.
    */
  def verify(definition: DeclaredTypeDefinition)(implicit registry: Registry): Verification = {
    definition match {
      case struct: StructDefinition => StructConstraints.verify(struct)
      case _ => Verification.succeed
    }
  }

}
