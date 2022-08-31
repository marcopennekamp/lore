package lore.compiler.transformation

import lore.compiler.semantics.expressions.Expression._
import lore.compiler.semantics.expressions.{Expression, ExpressionVerificationVisitor, ExpressionVisitor}

object UsageAnalyzer {

  /**
    * Analyzes whether each sub-expression in `expression` is used and sets their [[Expression.isUsed]]. `isUsed` of
    * `expression` is not changed, but taken into account for the usage analysis of the sub-expressions.
    */
  def analyzeUsage(expression: Expression): Unit = {
    val visitor = UsageVisitor()
    ExpressionVisitor.visit(visitor)(expression)
  }

  /**
    * For each visited expression, the usage visitor determines whether an expression's sub-expressions are unused. As
    * the default for `isUsed` is `true`, the visitor only has to set `isUsed` for unused expressions. Hence, many
    * cases can be omitted.
    */
  private case class UsageVisitor() extends ExpressionVerificationVisitor {
    override def before: PartialFunction[Expression, Unit] = {
      case expression@Block(expressions, _) =>
        // Only the last expression in a block can be used, and only if the block itself is used. A block that is typed
        // as `Unit` and contains a single expression `e` will actually be a block with two expressions: `e` and `()`
        // after type rehydration. Hence, `e.isUsed` will properly be set to `false` by this visitor.
        expressions.init.foreach(_.setUnused())
        if (expression.isUnused) {
          expressions.lastOption.foreach(_.setUnused())
        }

      // The bodies of cond cases will be unused if the cond itself is unused, because there is no need to assign a
      // result.
      case expression@Cond(cases, _) if expression.isUnused => cases.foreach(_.body.setUnused())

      // The body of a loop will be unused if the loop itself is unused, because there is no need to aggregate a
      // result list and hence no need to use the body's result.
      case expression@WhileLoop(_, body, _) if expression.isUnused => body.setUnused()
      case expression@ForLoop(_, body, _) if expression.isUnused => body.setUnused()

      case _ => // All sub-expressions are used by default or the expression has no sub-expressions.
    }
  }

}
