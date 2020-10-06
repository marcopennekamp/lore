package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.{LocalVariable, Registry, Scope, TypeScope, VariableScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.visitor.StmtVisitor
import lore.compiler.types.Type

object ExpressionTransformation {

  /**
    * Builds a semantic expression tree from the given expression node. Ensures that all other expression constraints
    * hold.
    */
  def transform(
    node: ExprNode,
    expectedType: Type,
    typeScope: TypeScope,
    variableScope: VariableScope,
  )(implicit registry: Registry): Compilation[Expression] = {
    // TODO: A block expected to return Unit should manually add a return value of () if the last expression's value
    //       isn't already that. Otherwise a, for example, function won't compile, because the last expression doesn't
    //       fit the expected return type:
    //          action foo() { concat([12], [15]) }  <-- doesn't compile (concat returns a list)

    for {
      _ <- ReturnConstraints.verify(node)
      visitor = new ExpressionTransformationVisitor(expectedType, typeScope, variableScope)
      expression <- StmtVisitor.visit(visitor)(node)
      _ <- verifyExpectedType(expression, expectedType)
    } yield expression
  }

  case class IllegallyTypedExpression(expression: Expression, expectedType: Type) extends Error(expression) {
    override def message: String = s"The expression $expression should return a value of type $expectedType, but actually returns" +
      s" a value of type ${expression.tpe}."
  }

  /**
    * Verifies that the expected result type is compatible with the type of the expression. If the actual type is not
    * compatible, it might be the case that all paths of the expression's last expression return a valid value. In such
    * a case, the expression is valid as well, because we can guarantee at compile-time that the right kind of value
    * is returned before the end of the expression is reached. This special case is also handled by this verification.
    */
  private def verifyExpectedType(expression: Expression, expectedType: Type): Verification = {
    if (expression.tpe <= expectedType || allPathsReturn(expression)) {
      Verification.succeed
    } else {
      Verification.fromErrors(Vector(IllegallyTypedExpression(expression, expectedType)))
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
    *   function foo(): Int = {
    *     return 5
    *     'You fool!'
    *   }
    */
  private def allPathsReturn(expression: Expression): Boolean = {
    expression match {
      case Expression.Return(_, _) => true
      case Expression.Block(expressions, _) => expressions.lastOption.exists(allPathsReturn)
      case Expression.IfElse(_, onTrue, onFalse, _, _) => allPathsReturn(onTrue) && allPathsReturn(onFalse)

      // Loops aren't guaranteed to run even once and so cannot guarantee that all paths end in a return.
      // Hence Expression.WhileLoop and Expression.ForLoop will also result in false.
      case _ => false
    }
  }

}
