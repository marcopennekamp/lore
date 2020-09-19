package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructDefinition

/**
  * Transpiles the class definition. It relies on the class type already being registered.
  */
class StructDefinitionTranspiler(definition: StructDefinition)(implicit registry: Registry) {
  def transpile(): Compilation[String] = {
    ???
  }
}
