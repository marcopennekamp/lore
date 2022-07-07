package lore.compiler.typing.synthesizer

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionInstance
import lore.compiler.types.FunctionType
import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.InferenceVariable.Assignments

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
        reporter.error(TypingFeedback.MultiFunctionValue.IllegalOutput(expression, expectedType, functionType))
        return None
      }
    }

    InferenceVariable.assign(expression.tpe.asInstanceOf[InferenceVariable], functionType, assignments)
  }

}
