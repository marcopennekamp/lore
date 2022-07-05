package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature, MultiFunctionDefinition}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.{BasicType, Fit}

object MultiFunctionDefinitionResolver {

  def initialize(mf: MultiFunctionDefinition)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    // TODO (multi-import): Rewrite this so that `fullName` can be deleted from `NamedDeclNode`.
    if (mf.functionNodes.exists(_.fullName != mf.name)) {
      val uniqueNames = mf.functionNodes.map(_.fullName).distinct
      throw CompilationException(s"The function nodes of a multi-function must all have the same name. Names:" +
        s" ${uniqueNames.mkString(", ")}. Multi-function name: ${mf.name}.")
    }

    val functions = mf.functionNodes.map(resolveFunction(_, mf))
    val uniqueFunctions = filterDuplicateFunctions(functions)
    mf.initialize(uniqueFunctions)
  }

  private def resolveFunction(
    node: DeclNode.FunctionNode,
    multiFunction: MultiFunctionDefinition,
  )(
    implicit registry: Registry,
    reporter: Reporter,
  ): FunctionDefinition = {
    Resolver.withTypeParameters(node.localModule, node.typeVariables) {
      implicit typeScope => implicit termScope => typeParameters =>
        val parameters = node.parameters.map(ParameterDefinitionResolver.resolve)
        val outputType = TypeExpressionEvaluator.evaluate(node.outputType).getOrElse(BasicType.Any)
        val signature = FunctionSignature(node.fullName, typeParameters, parameters, outputType, node.nameNode.position)
        new FunctionDefinition(signature, node, multiFunction)
    }
  }

  // TODO (multi-import): Move this error to the feedback package.
  case class FunctionAlreadyExists(definition: FunctionDefinition) extends Feedback.Error(definition) {
    override def message = s"The function `${definition.signature}` is already declared somewhere else or has a type-theoretic duplicate."
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
  private def filterDuplicateFunctions(
    functions: Vector[FunctionDefinition],
  )(implicit reporter: Reporter): Vector[FunctionDefinition] = {
    functions.flatMap { f1 =>
      val hasDuplicate = functions.filterNot(_ == f1).exists(
        f2 => Fit.areEquallySpecific(f2.signature.inputType, f1.signature.inputType)
      )
      if (!hasDuplicate) {
        Vector(f1)
      } else {
        reporter.error(FunctionAlreadyExists(f1))
        Vector.empty
      }
    }
  }

}
