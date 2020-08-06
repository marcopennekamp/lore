package lore.compiler.phases.transpilation

import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.NamedType

object TranspiledNames {
  def namedType(tpe: NamedType): String = s"lore_type_${tpe.name}"
  def classDefinition(definition: ClassDefinition): String = definition.name
  def temporaryVariable(name: String): String = s"lore_tmp_$name"
  def localVariable(loreName: String): String = s"lore_lv_$loreName"
}
