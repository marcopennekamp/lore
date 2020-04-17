package lore.definitions

import lore.compiler.Compilation._
import lore.compiler.Position
import lore.types.{ClassType, Type}

/**
  * The definition of both a class and an entity.
  *
  * @param localMembers The members declared within this class/entity. Does not include supertype properties.
  */
class ClassDefinition(
  override val name: String,
  override val tpe: ClassType,
  val localMembers: List[MemberDefinition[Type]],
  val constructors: List[ConstructorDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)
  override def verifyDeferredTypings: Verification = {
    (
      tpe.ownedBy.map(_.verifyType).toCompiledOption,
      localMembers.map(_.verifyType).simultaneous,
      constructors.flatMap(_.parameters).map(_.verifyType).simultaneous,
    ).simultaneous.map(_ => ())
  }

  /**
    * The list of all members belonging to this class, including superclass members.
    */
  lazy val members: List[MemberDefinition[Type]] = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers

  /**
    * The list of all properties belonging to this class, including superclass properties.
    */
  lazy val properties: List[PropertyDefinition] = members.flatMap { case property: PropertyDefinition => Some(property); case _ => None }

  /**
    * The list of all components belonging to this class, including superclass components.
    */
  lazy val components: List[ComponentDefinition] = members.flatMap { case component: ComponentDefinition => Some(component); case _ => None }

  /**
    * Whether this class is an entity.
    */
  def isEntity: Boolean = components.nonEmpty
}
