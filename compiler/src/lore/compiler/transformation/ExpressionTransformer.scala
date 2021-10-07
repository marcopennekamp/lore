package lore.compiler.transformation

import lore.compiler.feedback.TypingFeedback.SubtypeExpected
import lore.compiler.feedback.{MemoReporter, Reporter}
import lore.compiler.inference.Inference
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.scopes.{BindingScope, TypeScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.transformation2.ExpressionTransformationVisitor
import lore.compiler.types.{TupleType, Type}
import lore.compiler.typing.{Checker, Typing}

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
      if (!reporter.hasErrors) {
        val inferredTypes = Typing.check(expression, expectedType, label, reporter)
        val inferenceFailed = reporter.hasErrors

        val rehydrationVisitor = new TypeRehydrationVisitor(inferredTypes)
        val typedExpression = ExpressionVisitor.visit(rehydrationVisitor)(expression)

        val mutabilityVerifier = new MutabilityVerifier
        ExpressionVisitor.visit(mutabilityVerifier)(typedExpression)

        val builtinsVisitor = new BuiltinsVisitor
        val expressionWithBuiltins = ExpressionVisitor.visit(builtinsVisitor)(typedExpression)

        val expressionWithImplicitUnit = withImplicitUnitValue(expectedType)(expressionWithBuiltins)

        // Only verify the expected type if type inference actually completed. Otherwise, the expression's type will
        // likely be Any, which will always highlight the whole expression. This makes it almost impossible to see the
        // actual error.
        if (!inferenceFailed) {
          verifyExpectedType(expressionWithImplicitUnit, expectedType)
        }
        expressionWithImplicitUnit
      } else Expression.Hole(expectedType, node.position)
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

  /**
    * Verifies that the expected result type is compatible with the type of the expression. If the actual type is not
    * compatible, it might be the case that all paths of the expression's last expression return a valid value. In such
    * a case, the expression is valid as well, because we can guarantee at compile-time that the right kind of value
    * is returned before the end of the expression is reached. This special case is also handled by this verification.
    */
  private def verifyExpectedType(expression: Expression, expectedType: Type)(implicit reporter: Reporter): Unit = {
    if ((expression.tpe </= expectedType) && !allPathsReturn(expression)) {
      reporter.error(SubtypeExpected(expression.tpe, expectedType, expression))
    }
  }

  /**
    * Whether all paths that could be taken during the evaluation of the expression definitely end in a return.
    * If that is the case, and such an expression is the last expression in a block, we can be sure that the
    * block evaluates to the correct value regardless of the type of the actual expression.
    *
    * We only look at the last expression of a block to decide whether the returns suffice. That is only valid because
    * we combine it with dead code analysis, with dead code resulting in an error. A function like the following could
    * thus never be valid:
    *
    * {{{
    * function foo(): Int = {
    *   return 5
    *   'You fool!'
    * }
    * }}}
    */
  private def allPathsReturn(expression: Expression): Boolean = {
    expression match {
      case Expression.Return(_, _) => true
      case Expression.Block(expressions, _) => expressions.lastOption.exists(allPathsReturn)
      case cond@Expression.Cond(cases, _) => cond.isTotal && cases.forall(c => allPathsReturn(c.body))

      // Loops aren't guaranteed to run even once and so cannot guarantee that all paths end in a return.
      // Hence Expression.WhileLoop and Expression.ForLoop will also result in false.
      case _ => false
    }
  }

}
