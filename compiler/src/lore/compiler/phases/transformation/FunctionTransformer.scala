package lore.compiler.phases.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.scopes.FunctionBindingScope

object FunctionTransformer {

  /**
    * Compiles the function's body node and sets its `body` field. Also ensures that the return type of the signature
    * is sound compared to the result type of the body.
    */
  def transform(function: FunctionDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    function.bodyNode.foreach { node =>
      function.body = Some(
        ExpressionTransformer.transform(
          function.name,
          node,
          function.signature.outputType,
          function.getTypeScope(registry.typeScope),
          new FunctionBindingScope(function.signature, registry.bindingScope),
        )
      )
    }
  }

}
