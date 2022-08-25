package lore.compiler.typing2

import com.typesafe.scalalogging.Logger
import lore.compiler.semantics.expressions.Expression
import lore.compiler.utils.IndentationLogger

// TODO (multi-import): Check that important attempts and results are traced (especially call/function value typing).
//                      Refactor and overhaul all current traces.

object Typing2 {

  val indentationLogger: IndentationLogger = IndentationLogger("lore.compiler.typing")
  val logger: Logger = Logger(indentationLogger)
  val loggerBlank: Logger = Logger("lore.compiler.typing.blank")

  def traceExpressionType(expression: Expression, label: String, additional: String = ""): Unit = {
    logger.whenTraceEnabled {
      logger.trace(s"$label type ${expression.tpe} for `${expression.position.truncatedCode}`.$additional")
    }
  }

}
