package lore.compiler.phases.constraints

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{DeclaredSchemaDefinition, StructDefinition}
import lore.compiler.types.TypeVariable.Variance

object DeclaredSchemaConstraints {

  /**
    * Verifies:
    *   1. Co-/contra-/invariant type parameters must be used in appropriate positions in extended types.
    *   2. All struct constraints if the given definition is a struct.
    */
  def verify(definition: DeclaredSchemaDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyVariancePositions(definition)
    definition match {
      case struct: StructDefinition => StructConstraints.verify(struct)
      case _ =>
    }
  }

  /**
    * Verifies that co-/contra-/invariant type parameters are used in appropriate positions in extended types.
    */
  private def verifyVariancePositions(definition: DeclaredSchemaDefinition)(implicit reporter: Reporter): Unit = {
    definition.schema.supertypes.foreach {
      supertype => VarianceConstraints.verifyVariance(supertype, Variance.Covariant, definition.position)
    }
  }

}
