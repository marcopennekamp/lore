package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.StructSchema

class StructDefinition(
  override val name: String,
  override val schema: StructSchema,
  val properties: Vector[StructPropertyDefinition],
  override val position: Position,
) extends DeclaredSchemaDefinition {

  lazy val propertyMap: Map[String, StructPropertyDefinition] = properties.map(p => (p.name, p)).toMap

  lazy val openProperties: Vector[StructPropertyDefinition] = properties.filter(_.isOpen)

  lazy val constructor: StructConstructorDefinition = StructConstructorDefinition(this)

}
