package lore.compiler

import lore.ast.{DeclNode, TypeDeclNode}
import lore.definitions.{ConstructorDefinition, ParameterDefinition}

object FunctionDeclarationResolver {
/*
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry): FunctionDefinition = {
    val parameters = parameterDeclarations.map { decl =>
      Parameter(decl.name, evaluateTypeExpression(decl.tpe))
    }
    addFunction(LoreFunction(name, parameters, functionNode.isAbstract))
  }
*/
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