package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.Feedback
import lore.compiler.phases.resolution.ParameterDefinitionResolver.resolveParameterNode
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.DeclNode
import lore.compiler.types.Fit
import lore.compiler.utils.CollectionExtensions.VectorExtension

object MultiFunctionDefinitionResolver {

  def resolve(functionNodes: Vector[DeclNode.FunctionNode])(implicit typeScope: TypeScope): Compilation[MultiFunctionDefinition] = {
    if (!functionNodes.allEqual(_.name)) {
      val uniqueNames = functionNodes.map(_.name).distinct
      throw CompilationException(s"The function nodes of a multi-function don't all have the same name. Names: ${uniqueNames.mkString(", ")}.")
    }

    val name = functionNodes.head.name
    for {
      functions <- functionNodes.map(resolveFunction(_, typeScope)).simultaneous
      _ <- verifyFunctionsUnique(functions)
    } yield MultiFunctionDefinition(name, functions)
  }

  private def resolveFunction(node: DeclNode.FunctionNode, registryTypeScope: TypeScope): Compilation[FunctionDefinition] = {
    TypeVariableDeclarationResolver.resolve(node.typeVariables, registryTypeScope).flatMap { implicit typeScope =>
      (
        node.parameters.map(resolveParameterNode).simultaneous,
        TypeExpressionEvaluator.evaluate(node.outputType),
      ).simultaneous.map { case (parameters, outputType) =>
        val signature = FunctionSignature(node.name, parameters, outputType, node.position)
        new FunctionDefinition(signature, typeScope, node.body)
      }
    }
  }

  case class FunctionAlreadyExists(definition: FunctionDefinition) extends Feedback.Error(definition) {
    override def message = s"The function ${definition.signature} is already declared somewhere else or has a type-theoretic duplicate."
  }

  /**
    * Verifies that all functions declared in the multi-function have a unique signature, which means that their input
    * types aren't equally specific. If they are, multiple dispatch won't be able to differentiate between such two
    * functions, and hence they can't be valid.
    *
    * We find duplicates based on the Fit specificity of two functions.
    */
  private def verifyFunctionsUnique(functions: Vector[FunctionDefinition]): Verification = {
    functions.map { f1 =>
      val containsDuplicate = functions.filterNot(_ == f1).exists(f2 => Fit.isEquallySpecific(f2.signature.inputType, f1.signature.inputType))
      if (containsDuplicate) Compilation.fail(FunctionAlreadyExists(f1)) else Verification.succeed
    }.simultaneous.verification
  }

}
