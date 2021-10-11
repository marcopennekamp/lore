package lore.compiler.transformation

import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.scopes.{BindingScope, TypeScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.types.{TupleType, Type}
import lore.compiler.typing.Typing

object ExpressionTransformer {

  /**
    * Builds a semantic expression tree from the given AST expression node, performing type inference in the process.
    * Ensures that all other expression constraints hold.
    *
    * @param label An identifier string that is used during inference logging to make a specific function's inference
    *              logs more accessible.
    */
  def transform(
    node: ExprNode,
    expectedType: Type,
    typeScope: TypeScope,
    bindingScope: BindingScope,
    label: String,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    MemoReporter.nested(reporter) { implicit reporter =>
      val visitor = new ExpressionTransformationVisitor(typeScope, bindingScope)
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
          val expressionWithBuiltins = ExpressionVisitor.visit(builtinsVisitor)(typedExpression)

          withImplicitUnitValue(expectedType)(expressionWithBuiltins)
        }.getOrElse(hole)
      } else hole
    }
  }

  /**
    * For a block expression expected to return Unit, we have to manually add a unit return value if the block's value
    * isn't already a unit value.
    *
    * Example:
    * {{{
    * act test() {
    *   concat([12], [15]) // Should compile even though it returns a list.
    * }
    * }}}
    *
    * TODO (inference): This should be moved to a visitor such as [[TypeRehydrationVisitor]] (which operates
    *                   post-inference) so that this can be applied to all blocks, not just the top-most block. In
    *                   addition, we might also have to amend the Checker to turn the actual return type of a block
    *                   into `Unit` if the expected type is `Unit`.
    */
  private def withImplicitUnitValue(expectedType: Type)(expression: Expression): Expression = {
    expression match {
      case Expression.Block(expressions, position) if expectedType == TupleType.UnitType && expression.tpe != TupleType.UnitType =>
        Expression.Block(expressions :+ Expression.Tuple(Vector.empty, position), position)
      case _ => expression
    }
  }

}
