package lore.compiler.phases.verification

import lore.compiler.ast.visitor.{CombiningStmtVisitor, StmtVisitor, VerificationStmtVisitor}
import lore.compiler.ast.{ExprNode, StmtNode}
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.ReturnConstraints.{DeadCode, DefinitelyReturns, ImpossibleReturn, IsReturnAllowed}
import lore.compiler.core.{Compilation, Fragment}

private class ReturnDeadCodeVisitor()(implicit fragment: Fragment) extends CombiningStmtVisitor[DefinitelyReturns] {
  override def combine(list: List[DefinitelyReturns]): DefinitelyReturns = {
    if (list.isEmpty) false
    else list.forall(identity)
  }

  override def visit(node: StmtNode, returns: List[DefinitelyReturns]): Compilation[DefinitelyReturns] = node match {
    case StmtNode.ReturnNode(_) => Compilation.succeed(true)
    case ExprNode.BlockNode(statements) =>
      assert(statements.length == returns.length)

      // Check that a return statement isn't followed by any other code. If we have a "definitely returns" at any
      // point before the last element, this is such a point.
      if (returns.isEmpty) Compilation.succeed(false)
      else {
        val returnIndex = returns.init.indexOf(true)
        if (returnIndex >= 0) {
          val firstDeadNode = statements(returnIndex + 1)
          Compilation.fail(DeadCode(firstDeadNode))
        } else Compilation.succeed(returns.last)
      }
    case ExprNode.IfElseNode(_, _, _) =>
      // Ignore the condition.
      Compilation.succeed(returns.tail.forall(identity))
    case ExprNode.RepetitionNode(_, _) =>
      // Ignore the condition.
      Compilation.succeed(returns.last)
    case ExprNode.IterationNode(_, _) =>
      // Ignore the extractors.
      Compilation.succeed(returns.last)
    case _ => super.visit(node, returns)
  }
}

/**
  * Checks whether non-top-level expressions have a return. If that is the case, an error is returned.
  */
private class ReturnAllowedApplicator()(implicit fragment: Fragment)
  extends StmtVisitor.Applicator[Unit, IsReturnAllowed](new VerificationStmtVisitor { })
{
  override def handleMatch(node: StmtNode, isReturnAllowed: IsReturnAllowed): Compilation[Unit] = node match {
    case node@StmtNode.ReturnNode(expr) => visit(expr, false).flatMap { _ =>
      if (!isReturnAllowed) Compilation.fail(ImpossibleReturn(node)) else Verification.succeed
    }
    case ExprNode.BlockNode(statements) => statements.map(statement => visit(statement, isReturnAllowed)).simultaneous.verification
    case ExprNode.IfElseNode(condition, onTrue, onFalse) =>
      (visit(condition, false), visit(onTrue, isReturnAllowed), visit(onFalse, isReturnAllowed)).simultaneous.verification
    case ExprNode.RepetitionNode(condition, body) =>
      (visit(condition, false), visit(body, isReturnAllowed)).simultaneous.verification
    case ExprNode.IterationNode(extractors, body) =>
      (
        extractors.map {
          case ExprNode.ExtractorNode(_, collection) => visit(collection, false)
        }.simultaneous,
        visit(body, isReturnAllowed),
        ).simultaneous.verification
    case _ => super.handleMatch(node, false)
  }

  def verify(body: StmtNode): Verification =
    // At the top level of the function, we allow a return, of course. This is propagated to any children that can
    // be statements, but quickly gets stomped when an expression is expected.
    visit(body, true)
}

object ReturnConstraints {
  type DefinitelyReturns = Boolean
  type IsReturnAllowed = Boolean

  case class DeadCode(node: StmtNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"This node represents dead code after a previous return."
  }

  case class ImpossibleReturn(node: StmtNode.ReturnNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"You cannot return inside this expression."
  }

  /**
    * Verifies the following two constraints:
    * - Any return must not be followed by code in the same block. This effectively disallows dead code after
    *   a return statement.
    * - Constructions such as `if ({ return 0 }) a else b` are not allowed. Returning should not be
    *   possible from non-top-level expressions.
    */
  def verify(body: StmtNode)(implicit fragment: Fragment): Verification = {
    (
      StmtVisitor.visit(new ReturnDeadCodeVisitor())(body),
      new ReturnAllowedApplicator().visit(body, true)
    ).simultaneous.verification
  }
}
