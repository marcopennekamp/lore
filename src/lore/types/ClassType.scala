package lore.types

import lore.definitions.ClassDefinition
import lore.execution.Context

class ClassType(
  val supertype: Option[ClassType], val ownedBy: Option[Type], val isAbstract: Boolean
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * The list of component types belonging to the entity type.
    */
  lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)
  def isEntity: Boolean = this.definition.isEntity

  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.flatMap {
      case t: ClassType if t.supertype.contains(this) => Some(t)
      case _ => None
    }.toSet
  }
  override def verbose = s"${if (isAbstract) s"abstract class" else "class"} $toString extends ${supertype.getOrElse(AnyType)}"
}

