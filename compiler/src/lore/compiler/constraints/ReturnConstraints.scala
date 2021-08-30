package lore.compiler.constraints

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.constraints.ReturnConstraints.{DeadCode, DefinitelyReturns, ImpossibleReturn, IsReturnAllowed}
import lore.compiler.syntax.visitor.{CombiningTopLevelExprVisitor, TopLevelExprVisitor, VerificationTopLevelExprVisitor}
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}

object ReturnConstraints {
  type DefinitelyReturns = Boolean
  type IsReturnAllowed = Boolean

  case class DeadCode(node: TopLevelExprNode) extends Feedback.Error(node) {
    override def message = s"This node represents dead code after a previous return."
  }

  case class ImpossibleReturn(node: TopLevelExprNode.ReturnNode) extends Feedback.Error(node) {
    override def message = s"You cannot return inside this expression."
  }

  /**
    * Verifies the following two constraints:
    *
    * - Any return must not be followed by code in the same block. This effectively disallows dead code after a return
    *   top-level expression.
    * - Constructions such as `if ({ return 0 }) a else b` are not allowed. Returning should only be possible from
    *   top-level expressions.
    */
  def verify(body: TopLevelExprNode)(implicit reporter: Reporter): Unit = {
    verifyNoDeadCode(body)
    verifyReturnsAllowed(body)
  }

  private def verifyNoDeadCode(body: TopLevelExprNode)(implicit reporter: Reporter): Unit = {
    TopLevelExprVisitor.visit(new ReturnDeadCodeVisitor())(body)
  }

  private def verifyReturnsAllowed(body: TopLevelExprNode)(implicit reporter: Reporter): Unit = {
    new ReturnAllowedApplicator().visit(body, true)
  }
}

/**
  * Ensures that expressions cannot follow a `return` expression. Reports errors for any violating expressions.
  */
private class ReturnDeadCodeVisitor(implicit reporter: Reporter) extends CombiningTopLevelExprVisitor.Identity[DefinitelyReturns] {
  override def combine(returns: Vector[DefinitelyReturns]): DefinitelyReturns = {
    if (returns.isEmpty) false
    else returns.forall(identity)
  }

  override def visit(node: TopLevelExprNode, returns: Vector[DefinitelyReturns]): DefinitelyReturns = node match {
    case TopLevelExprNode.ReturnNode(_, _) => true

    case ExprNode.BlockNode(expressions, _) =>
      // Check that a return statement isn't followed by any other code. If we have a "definitely returns" at any
      // point before the last element, this is such a point.
      if (returns.isEmpty) false
      else {
        val returnIndex = returns.init.indexOf(true)
        if (returnIndex >= 0) {
          val firstDeadNode = expressions(returnIndex + 1)
          reporter.error(DeadCode(firstDeadNode))

          // If we report `true` here, the DeadCode error will potentially be reported multiple times. Hence, even
          // though there was a `return` expression, we don't want this to be reported up the chain.
          false
        } else returns.last
      }

    case ExprNode.IfElseNode(_, _, _, _) => returns.tail.forall(identity) // `tail` ignores the condition.
    case ExprNode.WhileNode(_, _, _) => returns.last // `last` ignores the condition.
    case ExprNode.ForNode(_, _, _) => returns.last // `last` ignores the condition.

    case _ => super.visit(node, returns)
  }
}

/**
  * Ensures that only top-level expressions contain a return. Reports errors for any violating expressions.
  */
private class ReturnAllowedApplicator(implicit reporter: Reporter)
  extends TopLevelExprVisitor.Applicator[Unit, IsReturnAllowed](new VerificationTopLevelExprVisitor { })
{
  override def handleMatch(node: TopLevelExprNode, isReturnAllowed: IsReturnAllowed): Unit = node match {
    case node@TopLevelExprNode.ReturnNode(expr, _) =>
      visit(expr, false)
      if (!isReturnAllowed) {
        reporter.error(ImpossibleReturn(node))
      }

    case ExprNode.BlockNode(expressions, _) =>
      expressions.foreach(statement => visit(statement, isReturnAllowed))

    case ExprNode.IfElseNode(condition, onTrue, onFalse, _) =>
      visit(condition, false)
      visit(onTrue, isReturnAllowed)
      onFalse.foreach(visit(_, isReturnAllowed))

    case ExprNode.WhileNode(condition, body, _) =>
      visit(condition, false)
      visit(body, isReturnAllowed)

    case ExprNode.ForNode(extractors, body, _) =>
      extractors.foreach {
        case ExprNode.ExtractorNode(_, collection, _) => visit(collection, false)
      }
      visit(body, isReturnAllowed)

    case _ => super.handleMatch(node, false)
  }
}
