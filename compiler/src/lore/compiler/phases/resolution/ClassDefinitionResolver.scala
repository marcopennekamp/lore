package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.functions.ConstructorDefinition
import lore.compiler.semantics.structures.{ClassDefinition, ComponentDefinition, MemberDefinition, PropertyDefinition}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.{ExprNode, TopLevelExprNode, TypeDeclNode}
import lore.compiler.types.{ClassType, TypeExpressionEvaluator}

object ClassDefinitionResolver {
  def resolve(node: TypeDeclNode.ClassNode)(implicit registry: Registry): Compilation[ClassDefinition] = {
    implicit val position: Position = node.position
    implicit val typeScope: TypeScope = registry.typeScope
    val classType = registry.getClassType(node.name).getOrElse(
      throw CompilationException(s"The class type for class ${node.name} should be registered by now!")
    )

    // Resolve the owned-by type, members, and constructors simultaneously for same-run error reporting.
    (
      node.ownedBy.map(TypeExpressionEvaluator.evaluate).toCompiledOption,
      node.members.map(resolveMember).simultaneous,
    ).simultaneous.flatMap { case (ownedBy, members) =>
      val definition = new ClassDefinition(node.name, classType, ownedBy, node.isEntity, members, node.position)
      classType.initialize(definition)
      node.constructors.map(resolveConstructor).simultaneous.map { constructors =>
        constructors.foreach(definition.registerConstructor)
        ensureDefaultConstructor(definition)
        definition.finalizeDefinition()
        definition
      }
    }
  }

  case class ComponentMustBeClass(node: TypeDeclNode.ComponentNode) extends Error(node) {
    override def message = s"The component ${node.name} is not a valid class type."
  }

  private def resolveMember(node: TypeDeclNode.MemberNode)(implicit typeScope: TypeScope): Compilation[MemberDefinition] = {
    node match {
      case TypeDeclNode.PropertyNode(name, tpe, isMutable, _) =>
        for {
          tpe <- TypeExpressionEvaluator.evaluate(tpe)
        } yield new PropertyDefinition(name, tpe, isMutable, node.position)
      case componentNode@TypeDeclNode.ComponentNode(name, overrides, _) =>
        for {
          tpe <- typeScope.resolve(name)(node.position)
            .require(_.isInstanceOf[ClassType])(ComponentMustBeClass(componentNode))
            .map(_.asInstanceOf[ClassType])
        } yield new ComponentDefinition(name, tpe, overrides, node.position)
    }
  }

  // TODO: Do we actually check that no two constructors of a given class share a name?

  def resolveConstructor(node: TypeDeclNode.ConstructorNode)(implicit registry: Registry): Compilation[ConstructorDefinition] = {
    implicit val typeScope: TypeScope = registry.typeScope
    node.parameters.map(ParameterDefinitionResolver.resolveParameterNode).simultaneous.map { parameters =>
      // TODO: Type variables from the class definition need to be available in the constructor context. We will also
      //       have to defer their loading, sadly, OR rethink WHAT we need to defer.
      new ConstructorDefinition(node.name, typeScope, parameters, node.body, node.position)
    }
  }

  /**
    * Ensures that the default constructor exists, generating and registering it if it does not.
    */
  private def ensureDefaultConstructor(classDefinition: ClassDefinition)(implicit registry: Registry): Unit = {
    if (classDefinition.getConstructor(classDefinition.name).isDefined) {
      return
    }

    // The parameters are all members as known by this class. Overridden components aren't part of the member list
    // and so, as expected, also not a parameter of the constructor. Rather, their overriding components become
    // parameters of the constructor.
    val parameters = classDefinition.members.map(_.asParameter)
    // The generated constructor doesn't have a position in the code, of course, but we still need to give generated
    // nodes a position. Thus, we default to the position of the class as a whole.
    implicit val position: Position = classDefinition.position
    // Now, we want a single construct continuation with the right arguments and superclass arguments. The only
    // tricky part is passing the overridden arguments twice. We do that by analyzing the superclass's default
    // constructor parameters, passing the correct overriding component if we encounter an overridden name.
    val arguments = classDefinition.localMembers.map(m => ExprNode.VariableNode(m.name, classDefinition.position))
    val withSuper = classDefinition.supertypeDefinition.map { superclass =>
      val superParameters = superclass.defaultConstructor.parameters
      val arguments = superParameters.map { superParameter =>
        // Either this component has been overridden and so must be fed from an argument which has also been
        // passed to the construct call, or we simply use the parameter name as the variable name, as they must
        // be equal for non-overridden members.
        ExprNode.VariableNode(classDefinition.overriddenToOverrider.getOrElse(superParameter.name, superParameter.name), position)
      }
      TopLevelExprNode.ConstructorCallNode(None, isSuper = true, arguments, position)
    }
    val body = ExprNode.BlockNode(List(TopLevelExprNode.ConstructNode(arguments, withSuper, position)), position)
    classDefinition.registerConstructor(
      new ConstructorDefinition(classDefinition.name, registry.typeScope, parameters, body, position)
    )
  }
}
