package lore.compiler.transformation

import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.types.Type
import lore.compiler.typing.Typing

object ExpressionTransformer {

  /**
    * Builds a semantic expression tree from the given AST expression node, performing type inference and usage
    * analysis in the process. Ensures that all other expression constraints hold.
    *
    * @param label An identifier string that is used during inference logging to make a specific function's inference
    *              logs more accessible.
    */
  def transform(
    node: ExprNode,
    expectedType: Type,
    typeScope: TypeScope,
    termScope: TermScope,
    label: String,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    MemoReporter.nested(reporter) { implicit reporter =>
      val visitor = new ExpressionTransformationVisitor(typeScope, termScope)
      val expression = TopLevelExprVisitor.visit(visitor)(node)

      // Only continue with the transformation if the visitor produced no errors. Otherwise, type inference might
      // report a lot of useless errors.
      val hole = Expression.Hole(expectedType, node.position)
      if (!reporter.hasErrors) {
        Typing.check(expression, expectedType, label, reporter).map { assignments =>
          val rehydrationVisitor = new TypeRehydrationVisitor(assignments)
          val typedExpression = ExpressionVisitor.visit(rehydrationVisitor)(expression)

          val mutabilityVerifier = new MutabilityVerifier
          ExpressionVisitor.visit(mutabilityVerifier)(typedExpression)

          val builtinsVisitor = new BuiltinsVisitor
          val builtinsExpression = ExpressionVisitor.visit(builtinsVisitor)(typedExpression)

          UsageAnalyzer.analyzeUsage(builtinsExpression)
          builtinsExpression
        }.getOrElse(hole)
      } else hole
    }
  }

}
