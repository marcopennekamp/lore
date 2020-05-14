package lore.compiler.phases.verification

import lore.ast.visitor.{CombiningStmtVisitor, StmtVisitor, VerificationStmtVisitor}
import lore.ast.{ExprNode, StmtNode}
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.ReturnConstraints.{DefinitelyReturns, IsReturnAllowed}
import lore.compiler.{Compilation, Fragment}

private case class DeadCode(node: ExprNode.BlockNode)(implicit fragment: Fragment) extends Error(node) {
  override def message = s"There is dead code after a return statement in this block."
}

private class ReturnDeadCodeVisitor()(implicit fragment: Fragment) extends CombiningStmtVisitor[DefinitelyReturns] {
  override def combine(list: List[DefinitelyReturns]): DefinitelyReturns = list.forall(identity)

  override def visit(node: StmtNode, returns: List[DefinitelyReturns]): Compilation[DefinitelyReturns] = node match {
    case StmtNode.ReturnNode(_) => Compilation.succeed(true)
    case node@ExprNode.BlockNode(_) =>
      // Check that a return statement isn't followed by any other code. If we have a "definitely returns" at any
      // point before the last element, this is such a point.
      if (returns.isEmpty) Compilation.succeed(false)
      else if (returns.init.exists(identity)) Compilation.fail(DeadCode(node))
      else Compilation.succeed(returns.last)
    case _ => super.visit(node, returns)
  }
}

case class ImpossibleReturn(node: StmtNode.ReturnNode)(implicit fragment: Fragment) extends Error(node) {
  override def message = s"You cannot return inside this expression."
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
    case ExprNode.RepeatWhileNode(condition, body, _) =>
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
