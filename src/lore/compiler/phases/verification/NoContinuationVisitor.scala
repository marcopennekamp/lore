package lore.compiler.phases.verification

import lore.ast.visitor.VerificationStmtVisitor
import lore.ast.{StmtNode, TopLevelExprNode}
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.NoContinuationVisitor.IllegalContinuation
import lore.compiler.{Compilation, Fragment}

class NoContinuationVisitor()(implicit fragment: Fragment) extends VerificationStmtVisitor {
  override def visitXary(node: StmtNode.XaryNode)(arguments: List[Unit]): Compilation[Unit] = node match {
    case node: TopLevelExprNode.ContinuationNode => Compilation.fail(IllegalContinuation(node))
    case _ => super.visitXary(node)(arguments)
  }
}

object NoContinuationVisitor {
  case class IllegalContinuation(node: TopLevelExprNode.ContinuationNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"A continuation is only valid as the very last top-level expression of a constructor block."
  }
}
