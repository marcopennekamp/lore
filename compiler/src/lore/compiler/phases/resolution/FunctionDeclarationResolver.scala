package lore.compiler.phases.resolution

import lore.compiler.ast.{DeclNode, TypeDeclNode}
import lore.compiler.core.Compilation._
import lore.compiler.core.{Compilation, Fragment, Registry}
import lore.compiler.definitions.{ConstructorDefinition, FunctionDefinition, ParameterDefinition, TypeScope, TypeVariableScope}
import lore.compiler.types.TypeExpressionEvaluator

object FunctionDeclarationResolver {
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry, fragment: Fragment): C[FunctionDefinition] = {
    // The fold ensures that the first type variable is registered before the second one is resolved, so that the first
    // one can be used as a bound of the second one, and so on.
    node.typeVariables.foldLeft(Compilation.succeed(new TypeVariableScope(registry.typeScope))) { case (compilation, node) =>
      compilation.flatMap { implicit typeScope =>
        TypeVariableDeclarationResolver.resolve(node).map { variable =>
          typeScope.register(variable, node.position)
          typeScope
        }
      }
    }.flatMap { implicit typeScope =>
      (
        // We verify parameter types right away, because all types should have been declared at this point.
        // Functions are resolved after all type declarations.
        node.parameters.map(resolveParameterNode).map(p => p.verifyDeferredTyping.map(_ => p)).simultaneous,
        TypeExpressionEvaluator.evaluate(node.outputType),
      ).simultaneous.map { case (parameters, outputType) =>
        new FunctionDefinition(node.name, typeScope, parameters, outputType, node.body, node.position)
      }
    }
  }

  def resolveConstructorNode(node: TypeDeclNode.ConstructorNode)(implicit registry: Registry, fragment: Fragment): ConstructorDefinition = {
    implicit val typeScope: TypeScope = registry.typeScope
    // TODO: Type variables from the class definition need to be available in the constructor context. We will also
    //       have to defer their loading, sadly, OR rethink WHAT we need to defer.
    ConstructorDefinition(
      node.name,
      registry.typeScope,
      node.parameters.map(resolveParameterNode),
      node.body,
      node.position,
    )
  }

  private def resolveParameterNode(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope, fragment: Fragment): ParameterDefinition = {
    new ParameterDefinition(node.name, () => TypeExpressionEvaluator.evaluate(node.tpe), node.position)
  }
}
