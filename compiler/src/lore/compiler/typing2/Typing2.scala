package lore.compiler.typing2

import lore.compiler.semantics.expressions.Expression
import lore.compiler.typing.Typing.logger

// TODO (multi-import): Check that important attempts and results are traced (especially call/function value typing).

object Typing2 {

  def traceExpressionType(expression: Expression, label: String, additional: String = ""): Unit = {
    logger.whenTraceEnabled {
      logger.trace(s"$label type ${expression.tpe} for `${expression.position.truncatedCode}`.$additional")
    }
  }

}
