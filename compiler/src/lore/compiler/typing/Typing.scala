package lore.compiler.typing

import com.typesafe.scalalogging.Logger
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType, Type}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.Checker
import lore.compiler.utils.IndentationLogger
import lore.compiler.utils.Timer.timed

object Typing {

  val indentationLogger: IndentationLogger = IndentationLogger("lore.compiler.typing")
  val logger: Logger = Logger(indentationLogger)
  val loggerBlank: Logger = Logger("lore.compiler.typing.blank")

  def check(expression: Expression, returnType: Type, label: String, parentReporter: Reporter)(implicit registry: Registry): Option[Assignments] = {
    logger.debug(s"Check types for `$label` at ${expression.position}:")

    val result = timed(s"Checking types for `$label`", log = s => logger.debug(s)) {
      MemoReporter.nested(parentReporter) { implicit reporter =>
        val checker = Checker(returnType)

        // TODO (inference): This feels like a hack. Is there another way to handle unit values? Perhaps in Checker
        //                   itself?
        val expectedType = if (returnType == TupleType.UnitType) BasicType.Any else returnType
        val assignmentsOption = checker.check(expression, expectedType, Map.empty)

        logger.whenDebugEnabled {
          assignmentsOption match {
            case Some(assignments) =>
              val prefix = s"Checking types for `$label` was successful"
              if (assignments.nonEmpty) {
                logger.debug(s"$prefix with the following assignments:\n${assignments.stringified}\n")
              } else {
                logger.debug(s"$prefix.\n")
              }

            case None =>
              logger.debug(s"Checking types for `$label` failed with the following feedback:")
              Feedback.logAll(reporter.feedback)
          }
        }

        assignmentsOption
      }
    }

    loggerBlank.debug("")
    result
  }

  def traceExpressionType(expression: Expression, assignments: Assignments, label: String, additional: String = ""): Unit = {
    logger.whenTraceEnabled {
      val inferredType = InferenceVariable.instantiateCandidate(expression.tpe, assignments)
      logger.trace(s"$label type $inferredType for `${expression.position.truncatedCode}`.$additional")
    }
  }

}
