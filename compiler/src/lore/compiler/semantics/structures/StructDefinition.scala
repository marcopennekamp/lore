package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.{GlobalModule, LocalModule}
import lore.compiler.types.StructSchema
import lore.compiler.utils.Once

class StructDefinition(
  override val name: NamePath,
  override val schema: StructSchema,
  val isObject: Boolean,
  val companionModule: Option[GlobalModule],
  override val localModule: LocalModule,
  override val position: Position,
) extends DeclaredSchemaDefinition {

  /**
    * Struct properties are resolved in a second step, as their properties may depend on types ordered later in the
    * schema resolution order.
    *
    * TODO (multi-import): The name `propertiesOnce` is quite awkward. Maybe hide the Once altogether.
    */
  val propertiesOnce: Once[Vector[StructPropertyDefinition]] = new Once
  def properties: Vector[StructPropertyDefinition] = propertiesOnce.value

  lazy val propertyMap: Map[String, StructPropertyDefinition] = properties.map(p => (p.name, p)).toMap

  lazy val openProperties: Vector[StructPropertyDefinition] = properties.filter(_.isOpen)

}
