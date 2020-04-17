package lore.types

import lore.compiler.Registry
import lore.definitions.ClassDefinition

class ClassType(
  val supertype: Option[ClassType], val ownedBy: Option[OwnedBy], val isAbstract: Boolean
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * The list of component types belonging to the entity type.
    */
  lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)
  def isEntity: Boolean = this.definition.isEntity

  override def directDeclaredSubtypes(implicit registry: Registry): Set[Type] = {
    // TODO: Replace with a calculation from a proper subtyping tree.
    registry.getTypes.values.flatMap {
      case t: ClassType if t.supertype.contains(this) => Some(t)
      case _ => None
    }.toSet
  }
  override def verbose = s"${if (isAbstract) s"abstract class" else "class"} $toString extends ${supertype.getOrElse(AnyType)}"
}
