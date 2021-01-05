package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Error
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object StructConstraints {

  /**
    * Verifies:
    *   1. Properties must be unique.
    *
    * TODO (shape): Once structs can implement shape types directly or indirectly, we have to add a check that all
    *               required properties are actually defined.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry): Verification = {
    verifyPropertiesUnique(definition)
  }

  case class DuplicateProperty(definition: StructDefinition, property: StructPropertyDefinition) extends Error(property) {
    override def message = s"The property ${property.name} is declared twice in the struct ${definition.name}."
  }

  /**
    * Verifies that this struct's properties are unique.
    */
  private def verifyPropertiesUnique(definition: StructDefinition): Verification = {
    definition.properties.requireUnique(_.name, property => DuplicateProperty(definition, property)).verification
  }

}
