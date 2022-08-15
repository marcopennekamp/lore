package lore.compiler.typing2

import lore.compiler.semantics.expressions.Expression
import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.Typing.logger

object Typing2 {

  def traceExpressionType(expression: Expression, label: String, additional: String = ""): Unit = {
    logger.whenTraceEnabled {
      logger.trace(s"$label type ${expression.tpe} for `${expression.position.truncatedCode}`.$additional")
    }
  }

}
