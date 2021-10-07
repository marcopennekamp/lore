package lore.compiler.typing

import lore.compiler.feedback.{Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionInstance
import lore.compiler.types.FunctionType

object MultiFunctionValueSynthesizer {

  /**
    * Once a function instance for a multi-function value has been found, this function checks the expected output type
    * (if present), and assigns the resulting function type to the expression's inference variable.
    */
  def handleFunctionInstance(
    instance: FunctionInstance,
    expression: Expression.MultiFunctionValue,
    expectedType: Option[FunctionType],
    assignments: Assignments,
  )(implicit reporter: Reporter): Option[Assignments] = {
    val functionType = instance.signature.functionType

    expectedType.foreach { expectedType =>
      if (functionType.output </= expectedType.output) {
        reporter.error(TypingFeedback2.MultiFunctionValues.IllegalOutput(expression, expectedType, functionType))
        return None
      }
    }

    Helpers.assign(expression.tpe.asInstanceOf[InferenceVariable], functionType, assignments)
  }

}
