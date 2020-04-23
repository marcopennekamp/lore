package lore.definitions

import lore.ast.ExprNode.VariableNode
import lore.ast.TopLevelExprNode.ConstructorCallNode
import lore.ast.{ExprNode, TopLevelExprNode}
import lore.compiler.feedback.Position
import lore.types.{ClassType, ProductType, Type}
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
  // TODO: We might be able to remove some of the lazy qualifiers.

  override def supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)

  /**
    * A map of overridden names pointing to the names of their overriding components.
    */
  private val overriddenToOverrider: Map[String, String] = localComponents.flatMap(m => m.overrides.map(_ -> m.name)).toMap

  /**
    * The list of all members belonging to this class, including superclass members. This list EXCLUDES overridden
    * components defined in superclasses.
    */
  lazy val members: List[MemberDefinition[Type]] = {
    val all = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers
    // Exclude overridden components.
    all.filterNot(m => overriddenToOverrider.contains(m.name))
  }

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

  /**
    * The list of all constructors, including the default constructor.
    */
  lazy val constructors: List[ConstructorDefinition] = defaultConstructor :: definedConstructors.filterNot(_.name == this.name)

  /**
    * The signature of the local construct function. This does not include any arguments to be passed to the
    * super type.
    */
  lazy val constructSignature: FunctionSignature = {
    val inputType = ProductType(localMembers.map(_.tpe))
    FunctionSignature("construct", inputType, ProductType.UnitType)
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
        ConstructorDefinition(this.name, parameters, body, position)
    }
  }
}
