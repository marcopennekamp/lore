package lore.compiler.phases.resolution

import lore.compiler.ast.{DeclNode, TypeDeclNode}
import lore.compiler.core.Compilation._
import lore.compiler.core.{Fragment, Registry, TypeScope}
import lore.compiler.functions
import lore.compiler.functions.{ConstructorDefinition, FunctionDefinition, ParameterDefinition}
import lore.compiler.types.TypeExpressionEvaluator

object FunctionDeclarationResolver {
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry, fragment: Fragment): C[FunctionDefinition] = {
    TypeVariableDeclarationResolver.resolve(node.typeVariables)(registry.typeScope, fragment).flatMap { implicit typeScope =>
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
    functions.ConstructorDefinition(
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
