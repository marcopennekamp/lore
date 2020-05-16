package lore.definitions

import lore.ast.ExprNode.VariableNode
import lore.ast.TopLevelExprNode.ConstructorCallNode
import lore.ast.{ExprNode, TopLevelExprNode}
import lore.compiler.feedback.Position
import lore.types.{ClassType, ComponentType, ProductType, Type}
import lore.utils.CollectionExtensions._

/**
  * The definition of both a class and an entity.
  *
  * @param localMembers The members declared within this class/entity. Does not include supertype properties.
  * @param definedConstructors The constructors explicitly declared within this class/entity. Does not include the default constructor.
  */
class ClassDefinition(
  override val name: String,
  override val tpe: ClassType,
  val isEntity: Boolean,
  val localMembers: List[MemberDefinition[Type]],
  definedConstructors: List[ConstructorDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {
  definedConstructors.foreach(_.associateWith(this))

  // Many of the members here are declared as vals. This is only possible because definitions are created according
  // to the inheritance hierarchy.

  override val supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)

  /**
    * The list of all properties belonging to this class, excluding superclass components.
    */
  val localProperties: List[PropertyDefinition] = localMembers.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this class, excluding superclass components.
    */
  val localComponents: List[ComponentDefinition] = localMembers.filterType[ComponentDefinition]

  /**
    * A map of overridden names pointing to the names of their overriding components.
    */
  private val overriddenToOverrider: Map[String, String] = localComponents.flatMap(m => m.overrides.map(_ -> m.name)).toMap

  /**
    * The list of all members belonging to this class, including superclass members. This list EXCLUDES overridden
    * components defined in superclasses.
    */
  val members: List[MemberDefinition[Type]] = {
    val all = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers
    // Exclude overridden components.
    all.filterNot(m => overriddenToOverrider.contains(m.name))
  }

  /**
    * The list of all properties belonging to this class, including superclass properties.
    */
  val properties: List[PropertyDefinition] = members.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this class, including superclass components.
    */
  val components: List[ComponentDefinition] = members.filterType[ComponentDefinition]

  /**
    * The list of all constructors, including the default constructor.
    */
  val constructors: List[ConstructorDefinition] = defaultConstructor :: definedConstructors.filterNot(_.name == this.name)

  /**
    * The signature of the local construct "function". This does not include any arguments to be passed to the
    * super type.
    */
  lazy val constructSignature: FunctionSignature = {
    FunctionSignature("construct", localMembers.map(_.asParameter), ProductType.UnitType)
  }

  /**
    * Returns the already declared default constructor or generates a new one.
    */
  lazy val defaultConstructor: ConstructorDefinition = {
    // Try to find a defined default constructor at first. If none can be found, we generate our own.
    definedConstructors.find(_.name == this.name) match {
      case Some(constructor) => constructor
      case None =>
        // The parameters are all members as known by this class. Overridden components aren't part of the member list
        // and so, as expected, also not a parameter of the constructor. Rather, their overriding components become
        // parameters of the constructor.
        val parameters = members.map(_.asParameter)
        // Now, we want a single construct continuation with the right arguments and superclass arguments. The only
        // tricky part is passing the overridden arguments twice. We do that by analyzing the superclass's default
        // constructor parameters, passing the correct overriding component if we encounter an overridden name.
        val arguments = localMembers.map(m => ExprNode.VariableNode(m.name))
        val withSuper = supertypeDefinition.map { superclass =>
          val superParameters = superclass.defaultConstructor.parameters
          val arguments = superParameters.map { superParameter =>
            // Either this component has been overridden and so must be fed from an argument which has also been
            // passed to the construct call, or we simply use the parameter name as the variable name, as they must
            // be equal for non-overridden members.
            VariableNode(overriddenToOverrider.getOrElse(superParameter.name, superParameter.name))
          }
          ConstructorCallNode(None, arguments)
        }
        val body = ExprNode.BlockNode(List(
          TopLevelExprNode.ConstructNode(arguments, withSuper)
        ))
        val constructor = ConstructorDefinition(this.name, parameters, body, position)
        constructor.associateWith(this)
        constructor
    }
  }

  /**
    * Attempts to find a constructor with the given name.
    */
  def getConstructor(name: String): Option[ConstructorDefinition] = constructors.find(_.name == name)

  /**
    * Returns all component types that this class has in common with the other given class.
    *
    * If two components match, but one is a supertype of the other, the supertype will be chosen.
    */
  def commonComponentTypes(other: ClassDefinition): List[ComponentType] = {
    components.map(_.tpe).flatMap { left =>
      val commonTypes = other.components.map(_.tpe).flatMap { right =>
        if (left <= right) Some(right)
        else if (left >= right) Some(left)
        else None
      }

      // There should only be exactly one type that the other entity has in common with this entity. If not, one of
      // the entities is violating the component subtyping hierarchy constraint.
      assert(commonTypes.size <= 1)

      commonTypes.map(ComponentType)
    }
  }
}
