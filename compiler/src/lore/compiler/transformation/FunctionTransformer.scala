package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition

object FunctionTransformer {

  /**
    * Compiles the function's body node and sets its `body` field. Also ensures that the return type of the signature
    * is sound compared to the result type of the body.
    */
  def transform(function: FunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    function.node.body match {
      case Some(bodyNode) => function.body.assign(
        Some(
          ExpressionTransformer.transform(
            bodyNode,
            function.signature.outputType,
            function.getTypeScope,
            function.getTermScope,
            function.name.toString,
          )
        )
      )
      case None => function.body.assign(None)
    }
  }

}
