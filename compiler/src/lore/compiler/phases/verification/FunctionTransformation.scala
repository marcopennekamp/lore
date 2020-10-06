package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.{FunctionScope, Registry}

/**
  * For a given function, builds a semantic expression tree from the body's abstract syntax tree. It infers and checks
  * expression types and checks all other constraints on expressions of that function's body.
  */
object FunctionTransformation {

  /**
    * Compiles the function's body node and sets its 'body' field. Also ensures that the return type of the signature
    * is sound compared to the result type of the body.
    */
  def transform(function: FunctionDefinition)(implicit registry: Registry): Verification = {
    SignatureConstraints.verify(function.signature).flatMap { _ =>
      val compiledBody = function.bodyNode.map { node =>
        ExpressionTransformation.transform(
          node,
          function.signature.outputType,
          function.typeScope,
          new FunctionScope(function.signature, registry.variableScope)
        )
      }.toCompiledOption

      compiledBody.map { body =>
        function.body = body
      }
    }
  }

}
