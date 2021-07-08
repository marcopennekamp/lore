package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.Feedback
import lore.compiler.phases.resolution.ParameterDefinitionResolver.resolveParameterNode
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.DeclNode
import lore.compiler.types.Fit
import lore.compiler.utils.CollectionExtensions.VectorExtension

object MultiFunctionDefinitionResolver {

  def resolve(functionNodes: Vector[DeclNode.FunctionNode])(implicit typeScope: TypeScope): Compilation.Result[MultiFunctionDefinition] = {
    if (!functionNodes.allEqual(_.name)) {
      val uniqueNames = functionNodes.map(_.name).distinct
      throw CompilationException(s"The function nodes of a multi-function don't all have the same name. Names: ${uniqueNames.mkString(", ")}.")
    }

    val name = functionNodes.head.name
    for {
      functions <- functionNodes.map(resolveFunction(_, typeScope)).simultaneous
      functions <- filterDuplicateFunctions(functions)
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
    * Filters out all duplicate functions declared in the multi-function, producing an error for each such function. A
    * duplicate function is a function f1 for which another function f2 exists that has an equally specific input type.
    * Multiple dispatch is unable to differentiate between two functions with equally specific input types, which makes
    * them invalid.
    *
    * If two functions f1 and f2 are duplicates of each other, we have to filter out both because we don't know whether
    * the programmer made their error with f1 or f2.
    */
  private def filterDuplicateFunctions(functions: Vector[FunctionDefinition]): Compilation.Result[Vector[FunctionDefinition]] = {
    functions.map { f1 =>
      val containsDuplicate = functions.filterNot(_ == f1).exists(f2 => Fit.isEquallySpecific(f2.signature.inputType, f1.signature.inputType))
      if (containsDuplicate) Compilation.fail(FunctionAlreadyExists(f1)) else Compilation.succeed(f1)
    }.simultaneous
  }

}
