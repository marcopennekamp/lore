package lore.compiler.phases.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, Fit}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object MultiFunctionDefinitionResolver {

  def resolve(functionNodes: Vector[DeclNode.FunctionNode])(implicit typeScope: TypeScope, reporter: Reporter): MultiFunctionDefinition = {
    if (!functionNodes.allEqual(_.name)) {
      val uniqueNames = functionNodes.map(_.name).distinct
      throw CompilationException(s"The function nodes of a multi-function don't all have the same name. Names: ${uniqueNames.mkString(", ")}.")
    }

    val name = functionNodes.head.name
    val functions = functionNodes.map(resolveFunction(_, typeScope))
    val uniqueFunctions = filterDuplicateFunctions(functions)
    MultiFunctionDefinition(name, uniqueFunctions)
  }

  private def resolveFunction(node: DeclNode.FunctionNode, registryTypeScope: TypeScope)(implicit reporter: Reporter): FunctionDefinition = {
    val typeParameters = TypeVariableDeclarationResolver.resolve(node.typeVariables, registryTypeScope)
    implicit val typeScope: TypeScope = ImmutableTypeScope.from(typeParameters, registryTypeScope)
    val parameters = node.parameters.map(ParameterDefinitionResolver.resolve)
    val outputType = TypeExpressionEvaluator.evaluate(node.outputType).getOrElse(BasicType.Any)
    val signature = FunctionSignature(node.name, parameters, outputType, node.nameNode.position)
    new FunctionDefinition(signature, typeParameters, node.body)
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
  private def filterDuplicateFunctions(functions: Vector[FunctionDefinition])(implicit reporter: Reporter): Vector[FunctionDefinition] = {
    functions.flatMap { f1 =>
      val hasDuplicate = functions.filterNot(_ == f1).exists(f2 => Fit.isEquallySpecific(f2.signature.inputType, f1.signature.inputType))
      if (!hasDuplicate) {
        Vector(f1)
      } else {
        reporter.error(FunctionAlreadyExists(f1))
        Vector.empty
      }
    }
  }

}
