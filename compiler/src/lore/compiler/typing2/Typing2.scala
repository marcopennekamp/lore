package lore.compiler.typing2

import com.typesafe.scalalogging.Logger
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.types.Type
import lore.compiler.utils.IndentationLogger
import lore.compiler.utils.Timer.timed

object Typing2 {

  val indentationLogger: IndentationLogger = IndentationLogger("lore.compiler.typing")
  val logger: Logger = Logger(indentationLogger)
  val loggerBlank: Logger = Logger("lore.compiler.typing.blank")

  def check(
    expression: UntypedExpression,
    returnType: Type,
    label: String,
    parentReporter: Reporter,
  )(implicit registry: Registry): Option[Expression] = {
    logger.debug(s"Check types for `$label` at ${expression.position}:")

    val result = timed(s"Checking types for `$label`", log = s => logger.debug(s)) {
      MemoReporter.nested(parentReporter) { implicit reporter =>
        val result = Checker2.check(expression, returnType, InferenceContext(returnType, Map.empty))

        logger.whenDebugEnabled {
          result match {
            case Some((typedExpression, _)) =>
              logger.debug(s"Checking types for `$label` was successful with the following expression type:" +
                s" ${typedExpression.tpe}.")

            case None =>
              logger.debug(s"Checking types for `$label` failed with the following feedback:")
              Feedback.logAll(reporter.feedback)
          }
        }

        result
      }
    }

    loggerBlank.debug("")
    result.map(_._1)
  }

  def traceExpressionType(
    expression: Expression,
    label: String,
    additional: String = "",
  ): Unit = logger.whenTraceEnabled{
    logger.trace(s"$label type ${expression.tpe} for `${expression.position.truncatedCode}`.$additional")
  }

  def traceCheckOrInfer(
    label: String,
    expression: UntypedExpression,
    expectedType: Option[Type],
  ): Unit = logger.whenTraceEnabled {
    val mode = expectedType.map(_ => "Check").getOrElse("Infer")
    val expectedTypeInfo = expectedType.map(t => s" with expected output type `$t`").getOrElse("")
    logger.trace(s"$mode $label `${expression.position.truncatedCode}`$expectedTypeInfo:")
  }

}
