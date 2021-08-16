package lore.compiler.inference

import lore.compiler.inference.Inference.isFullyInstantiated
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.Type

object FunctionInference {

  /**
    * Returns a list of judgments for inferring argument types as well as parameter inference variables.
    *
    * For each argument/parameter pair, we add two typing judgments:
    *   - A Fits judgment ensures that any type variables on the parameter side (represented by inference variables)
    *     are properly assigned their bounds. (If the parameter type contains inference variables.)
    *   - A Subtypes judgment allows inference of an argument's type based on the parameter type.
    *
    * Each argument/parameter pair receives its own Subtypes and possibly Fits judgments so that the inference
    * algorithm can be maximally flexible in the order of resolution.
    */
  def argumentJudgments(arguments: Vector[Expression], parameterTypes: Vector[Type]): Vector[TypingJudgment] = {
    arguments.zip(parameterTypes).flatMap {
      case (argument, parameterType) =>
        var result: Vector[TypingJudgment] = Vector(
          TypingJudgment.Subtypes(argument.tpe, parameterType, argument.position)
        )

        if (!isFullyInstantiated(parameterType)) {
          result = TypingJudgment.Fits(argument.tpe, parameterType, argument.position) +: result
        }

        result
    }
  }

}
