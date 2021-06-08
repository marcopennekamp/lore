package lore.compiler.phases.transformation

import lore.compiler.core.Position
import lore.compiler.phases.transformation.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{CallTarget, MultiFunctionDefinition}

object FunctionTyping {

  /**
    * Builds a simple multi-function call and the corresponding inference judgments.
    */
  def multiFunctionCall(
    mf: MultiFunctionDefinition,
    arguments: Vector[Expression],
    position: Position,
  )(implicit registry: Registry): (Expression.Call, Vector[TypingJudgment]) = {
    val resultType = new InferenceVariable
    (
      Expression.Call(CallTarget.MultiFunction(mf), arguments, resultType, position),
      Vector(
        TypingJudgment.MultiFunctionHint(mf, arguments, position),
        TypingJudgment.MultiFunctionCall(resultType, mf, arguments.map(_.tpe), position),
      )
    )
  }

}
