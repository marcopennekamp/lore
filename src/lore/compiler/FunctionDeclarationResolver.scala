package lore.compiler

import lore.ast.{DeclNode, TypeDeclNode}
import lore.definitions.{ConstructorDefinition, FunctionDefinition, ParameterDefinition}
import lore.compiler.Compilation._

object FunctionDeclarationResolver {
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry, fragment: Fragment): C[FunctionDefinition] = {
    (
      // We verify parameter types right away, because all types should have been declared at this point. Functions are
      // resolved after all type declarations.
      node.parameters.map(resolveParameterNode).map(p => p.verifyType.map(_ => p)).simultaneous,
      TypeExpressionEvaluator.evaluate(node.outputType),
    ).simultaneous.map { case (parameters, outputType) =>
      FunctionDefinition(node.name, parameters, outputType, node.body, node.position)
    }
  }

  def resolveConstructorNode(node: TypeDeclNode.ConstructorNode)(implicit registry: Registry, fragment: Fragment): ConstructorDefinition = {
    ConstructorDefinition(
      node.name,
      node.parameters.map(resolveParameterNode),
      node.body,
      node.position,
    )
  }

  private def resolveParameterNode(node: DeclNode.ParameterNode)(implicit registry: Registry, fragment: Fragment): ParameterDefinition = {
    new ParameterDefinition(node.name, () => TypeExpressionEvaluator.evaluate(node.tpe), node.position)
  }
}
