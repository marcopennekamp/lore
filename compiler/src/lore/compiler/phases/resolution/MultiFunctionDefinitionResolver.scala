package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.resolution.DeclarationResolver.FunctionAlreadyExists
import lore.compiler.phases.resolution.ParameterDefinitionResolver.resolveParameterNode
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TypeExpressionEvaluator

object MultiFunctionDefinitionResolver {
  def resolve(functionNodes: List[DeclNode.FunctionNode])(implicit registry: Registry): Compilation[MultiFunctionDefinition] = {
    // Of course, all functions added to the multi-function must have the same name. If that is not the case,
    // there is something very wrong with the compiler.
    if (functionNodes.size > 1) {
      if (!functionNodes.sliding(2).forall { case List(a, b) => a.name == b.name }) {
        throw CompilationException("The function nodes of a multi-function don't all have the same name.")
      }
    }

    val name = functionNodes.head.name
    for {
      functions <- functionNodes.map(resolveFunction).simultaneous
      _ <- verifyFunctionsUnique(functions)
    } yield MultiFunctionDefinition(name, functions)
  }

  private def resolveFunction(node: DeclNode.FunctionNode)(implicit registry: Registry): Compilation[FunctionDefinition] = {
    TypeVariableDeclarationResolver.resolve(node.typeVariables, registry.typeScope).flatMap { implicit typeScope =>
      (
        node.parameters.map(resolveParameterNode).simultaneous,
        TypeExpressionEvaluator.evaluate(node.outputType),
        ).simultaneous.map { case (parameters, outputType) =>
        new FunctionDefinition(node.name, typeScope, parameters, outputType, node.body, node.position)
      }
    }
  }

  /**
    * Verifies that all functions declared in the multi-function have a unique signature, which means that their
    * input types aren't equally specific. If they are, multiple dispatch won't be able to differentiate between
    * such two functions, and hence they can't be valid.
    */
  private def verifyFunctionsUnique(functions: List[FunctionDefinition]): Verification = {
    functions.map { function =>
      // We decide "duplicity" based on the specificity two functions would have in a multi-function fit context.
      // That is, if two functions are equally specific, they are effectively the same in the eyes of multiple
      // dispatch. This is what we want to avoid by verifying that all functions are "unique".
      val containsDuplicate = functions.filterNot(_ == function).exists(_.signature.isEquallySpecific(function.signature))
      if (containsDuplicate) {
        Compilation.fail(FunctionAlreadyExists(function))
      } else {
        Verification.succeed
      }
    }.simultaneous.verification
  }
}
