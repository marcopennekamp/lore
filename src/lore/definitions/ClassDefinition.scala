package lore.definitions

import lore.compiler.feedback.Position
import lore.types.{ClassType, Type}
import lore.utils.CollectionExtensions._

/**
  * The definition of both a class and an entity.
  *
  * @param localMembers The members declared within this class/entity. Does not include supertype properties.
  */
class ClassDefinition(
  override val name: String,
  override val tpe: ClassType,
  val isEntity: Boolean,
  val localMembers: List[MemberDefinition[Type]],
  val constructors: List[ConstructorDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)

  /**
    * The list of all members belonging to this class, including superclass members.
    */
  lazy val members: List[MemberDefinition[Type]] = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers

  /**
    * The list of all properties belonging to this class, excluding superclass components.
    */
  lazy val localProperties: List[PropertyDefinition] = localMembers.filterType[PropertyDefinition]

  /**
    * The list of all properties belonging to this class, including superclass properties.
    */
  lazy val properties: List[PropertyDefinition] = members.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this class, excluding superclass components.
    */
  lazy val localComponents: List[ComponentDefinition] = localMembers.filterType[ComponentDefinition]

  /**
    * The list of all components belonging to this class, including superclass components.
    */
  lazy val components: List[ComponentDefinition] = members.filterType[ComponentDefinition]
}
