package lore.compiler.typing

import lore.compiler.feedback.{Feedback, MemoReporter, Reporter}
import lore.compiler.inference.Inference.{Assignments, AssignmentsExtension, logger, loggerBlank}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType, Type}
import lore.compiler.utils.Timer.timed

object Typing {

  def check(expression: Expression, returnType: Type, label: String, parentReporter: Reporter)(implicit registry: Registry): Assignments = {
    logger.debug(s"Check types for $label at ${expression.position}:")

    val result = timed(s"Checking types for $label", log = s => logger.debug(s)) {
      MemoReporter.nested(parentReporter) { implicit reporter =>
        val checker = Checker(returnType)

        // TODO (inference): This feels like a hack. Is there another way to handle unit values? Perhaps in Checker
        //                   itself?
        val expectedType = if (returnType == TupleType.UnitType) BasicType.Any else returnType
        val assignments = checker.check(expression, expectedType, Map.empty)

        logger.whenDebugEnabled {
          if (!reporter.hasErrors) {
            val prefix = s"Checking types for $label was successful"
            if (assignments.nonEmpty) {
              logger.debug(s"$prefix with the following assignments:\n${assignments.stringified}\n")
            } else {
              logger.debug(s"$prefix.\n")
            }
          } else {
            logger.debug(s"Checking types for $label failed with the following feedback:")
            Feedback.logAll(reporter.feedback)
          }
        }

        assignments
      }
    }

    loggerBlank.debug("")
    result
  }

}
