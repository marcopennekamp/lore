package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.structures.SchemaDefinition
import lore.compiler.utils.Once

trait NamedSchema extends TypeSchema {
  def name: NamePath
}

object NamedSchema {
  trait DefinitionProperty[Def <: SchemaDefinition] extends NamedSchema {
    private val _definition: Once[Def] = new Once
    def definition: Def = _definition.value
    def initialize(definition: Def): Unit = _definition.assign(definition)
  }
}
