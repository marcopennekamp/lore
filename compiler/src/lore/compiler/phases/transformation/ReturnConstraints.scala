package lore.compiler.phases.transformation

import lore.compiler.syntax.visitor.{CombiningTopLevelExprVisitor, TopLevelExprVisitor, VerificationTopLevelExprVisitor}
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.core.{Compilation, Error}
import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.phases.transformation.ReturnConstraints.{DeadCode, DefinitelyReturns, ImpossibleReturn, IsReturnAllowed}

object ReturnConstraints {
  type DefinitelyReturns = Boolean
  type IsReturnAllowed = Boolean

  case class DeadCode(node: TopLevelExprNode) extends Error(node) {
    override def message = s"This node represents dead code after a previous return."
  }

  case class ImpossibleReturn(node: TopLevelExprNode.ReturnNode) extends Error(node) {
    override def message = s"You cannot return inside this expression."
  }

  /**
    * Verifies the following two constraints:
    * - Any return must not be followed by code in the same block. This effectively disallows dead code after
    *   a return TLE.
    * - Constructions such as `if ({ return 0 }) a else b` are not allowed. Returning should not be
    *   possible from non-top-level expressions.
    *
    * These constraints should be verified before function transformation so that we can operate the transformation
    * under tighter constraints.
    */
  def verify(body: TopLevelExprNode): Verification = {
    (
      TopLevelExprVisitor.visit(new ReturnDeadCodeVisitor())(body),
      new ReturnAllowedApplicator().visit(body, true)
    ).simultaneous.verification
  }
}

private class ReturnDeadCodeVisitor() extends CombiningTopLevelExprVisitor[DefinitelyReturns] {
  override def combine(returns: Vector[DefinitelyReturns]): DefinitelyReturns = {
    if (returns.isEmpty) false
    else returns.forall(identity)
  }

  override def visit(node: TopLevelExprNode, returns: Vector[DefinitelyReturns]): Compilation[DefinitelyReturns] = node match {
    case TopLevelExprNode.ReturnNode(_, _) => true.compiled
    case ExprNode.BlockNode(expressions, _) =>
      assert(expressions.length == returns.length)

      // Check that a return statement isn't followed by any other code. If we have a "definitely returns" at any
      // point before the last element, this is such a point.
      if (returns.isEmpty) false.compiled
      else {
        val returnIndex = returns.init.indexOf(true)
        if (returnIndex >= 0) {
          val firstDeadNode = expressions(returnIndex + 1)
          Compilation.fail(DeadCode(firstDeadNode))
        } else returns.last.compiled
      }
    case ExprNode.IfElseNode(_, _, _, _) =>
      // Ignore the condition.
      returns.tail.forall(identity).compiled
    case ExprNode.WhileNode(_, _, _) =>
      // Ignore the condition.
      returns.last.compiled
    case ExprNode.ForNode(_, _, _) =>
      // Ignore the extractors.
      returns.last.compiled
    case _ => super.visit(node, returns)
  }
}

/**
  * Checks whether non-top-level expressions have a return. If that is the case, an error is returned.
  */
private class ReturnAllowedApplicator()
  extends TopLevelExprVisitor.Applicator[Unit, IsReturnAllowed](new VerificationTopLevelExprVisitor { })
{
  override def handleMatch(node: TopLevelExprNode, isReturnAllowed: IsReturnAllowed): Compilation[Unit] = node match {
    case node@TopLevelExprNode.ReturnNode(expr, _) => visit(expr, false).flatMap { _ =>
      if (!isReturnAllowed) Compilation.fail(ImpossibleReturn(node)) else Verification.succeed
    }
    case ExprNode.BlockNode(expressions, _) => expressions.map(statement => visit(statement, isReturnAllowed)).simultaneous.verification
    case ExprNode.IfElseNode(condition, onTrue, onFalse, _) =>
      (visit(condition, false), visit(onTrue, isReturnAllowed), visit(onFalse, isReturnAllowed)).simultaneous.verification
    case ExprNode.WhileNode(condition, body, _) =>
      (visit(condition, false), visit(body, isReturnAllowed)).simultaneous.verification
    case ExprNode.ForNode(extractors, body, _) =>
      (
        extractors.map {
          case ExprNode.ExtractorNode(_, collection, _) => visit(collection, false)
        }.simultaneous,
        visit(body, isReturnAllowed),
        ).simultaneous.verification
    case _ => super.handleMatch(node, false)
  }

  def verify(body: TopLevelExprNode): Verification = {
    // At the top level of the function, we allow a return, of course. This is propagated to any children that can
    // be TLEs, but quickly gets stomped when an expression is expected.
    visit(body, true)
  }
}
