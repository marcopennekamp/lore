package lore.compiler.phases.resolution

import lore.compiler.ast.{DeclNode, TypeDeclNode}
import lore.compiler.Compilation._
import lore.compiler.{Fragment, Registry}
import lore.compiler.definitions.{ConstructorDefinition, FunctionDefinition, ParameterDefinition}
import lore.compiler.types.TypeExpressionEvaluator

object FunctionDeclarationResolver {
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry, fragment: Fragment): C[FunctionDefinition] = {
    (
      // We verify parameter types right away, because all types should have been declared at this point. Functions are
      // resolved after all type declarations.
      // TODO: We need to build a sort of type variable context and then pass it to verifyDeferredTyping of the
      //       parameter node as well as the TypeExpressionEvaluator to evaluate the output type... We need to
      //       take care that a variable of the same name is represented by the same instance.
      node.parameters.map(resolveParameterNode).map(p => p.verifyDeferredTyping.map(_ => p)).simultaneous,
      TypeExpressionEvaluator.evaluate(node.outputType),
    ).simultaneous.map { case (parameters, outputType) =>
      new FunctionDefinition(node.name, parameters, outputType, node.body, node.position)
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
