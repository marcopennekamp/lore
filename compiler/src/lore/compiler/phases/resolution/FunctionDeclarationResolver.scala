package lore.compiler.phases.resolution

import lore.compiler.syntax.{DeclNode, ExprNode, TypeDeclNode}
import lore.compiler.core.Compilation._
import lore.compiler.core.CompilationException
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.semantics.functions.{ConstructorDefinition, FunctionDefinition, ParameterDefinition}
import lore.compiler.types.TypeExpressionEvaluator

object FunctionDeclarationResolver {
  def resolveFunctionNode(node: DeclNode.FunctionNode)(implicit registry: Registry): C[FunctionDefinition] = {
    TypeVariableDeclarationResolver.resolve(node.typeVariables, registry.typeScope).flatMap { implicit typeScope =>
      (
        // We verify parameter types right away, because all types should have been declared at this point.
        // Functions are resolved after all type declarations.
        node.parameters.map(resolveParameterNode).map(p => p.verifyDeferredTyping.map(_ => p)).simultaneous,
        TypeExpressionEvaluator.evaluate(node.outputType),
      ).simultaneous.flatMap { case (parameters, outputType) =>
        node.body.map(FunctionTransformations.transformBody).toCompiledOption.map { body =>
          new FunctionDefinition(node.name, typeScope, parameters, outputType, body, node.position)
        }
      }
    }
  }

  def resolveConstructorNode(node: TypeDeclNode.ConstructorNode)(implicit registry: Registry): C[ConstructorDefinition] = {
    implicit val typeScope: TypeScope = registry.typeScope
    // TODO: Type variables from the class definition need to be available in the constructor context. We will also
    //       have to defer their loading, sadly, OR rethink WHAT we need to defer.
    FunctionTransformations.transformBody(node.body).map {
      case body: ExprNode.BlockNode =>
        ConstructorDefinition(
          node.name,
          registry.typeScope,
          node.parameters.map(resolveParameterNode),
          body,
          node.position,
        )
      case _ => throw CompilationException("A constructor body must be transformed to a block node.")
    }
  }

  private def resolveParameterNode(node: DeclNode.ParameterNode)(implicit typeScope: TypeScope): ParameterDefinition = {
    new ParameterDefinition(node.name, () => TypeExpressionEvaluator.evaluate(node.tpe), node.position)
  }
}
