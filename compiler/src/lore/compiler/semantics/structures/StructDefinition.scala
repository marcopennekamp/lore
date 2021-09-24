package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.{GlobalModule, LocalModule}
import lore.compiler.types.StructSchema

class StructDefinition(
  override val name: NamePath,
  override val schema: StructSchema,
  val properties: Vector[StructPropertyDefinition],
  val isObject: Boolean,
  val companionModule: Option[GlobalModule],
  override val localModule: LocalModule,
  override val position: Position,
) extends DeclaredSchemaDefinition {

  lazy val propertyMap: Map[String, StructPropertyDefinition] = properties.map(p => (p.name, p)).toMap

  lazy val openProperties: Vector[StructPropertyDefinition] = properties.filter(_.isOpen)

  /**
    * Whether the default values of this struct's properties are all localized. If not, and if the struct is an object,
    * the object's instance must be initialized lazily.
    */
  lazy val allDefaultsLocalized: Boolean = {
    properties.forall(_.defaultValue.forall(_.expression.isLocalized))
  }

}
