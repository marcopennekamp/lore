package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{ExpressionFeedback, Reporter}
import lore.compiler.inference.{FunctionInference, InferenceVariable, TypingJudgment}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.{FunctionType, TupleType}

object CallTransformation {

  /**
    * Builds a function value call from the given target and arguments.
    */
  def valueCall(target: Expression, arguments: Vector[Expression], position: Position)(implicit judgmentCollector: JudgmentCollector, reporter: Reporter): Expression.Call = {
    // A call target must be a value with a function type.
    val (inputType, outputType) = target.tpe match {
      // If the target's type is defined now, we can take a shortcut, because it's definitely a function.
      case targetType@FunctionType(input, output) =>
        if (arguments.length != input.elements.length) {
          reporter.error(ExpressionFeedback.ValueCall.IllegalArity(target, targetType, arguments.length, position))
        }
        (input, output)

      // May or may not be a function type, so we have to make sure that the type is even a function. The Assign
      // judgments ensure that the target's type is even a function type. For now, we don't want to infer the type of
      // the target based on the provided arguments, so we're relying on one-way type inference.
      case _ =>
        val inputType = TupleType(arguments.map(_ => new InferenceVariable))
        val outputType = new InferenceVariable

        judgmentCollector.add(TypingJudgment.Assign(FunctionType(inputType, outputType), target.tpe, target.position))

        (inputType, outputType)
    }

    judgmentCollector.add(
      FunctionInference.argumentJudgments(arguments, inputType.elements)
    )

    Expression.Call(CallTarget.Value(target), arguments, outputType, position)
  }

}
