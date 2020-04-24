package lore.compiler.phases.verification

import lore.ast.visitor.VerificationStmtVisitor
import lore.ast.{StmtNode, TopLevelExprNode}
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.NoContinuationVisitor.IllegalContinuation
import lore.compiler.{Compilation, Fragment}

class NoContinuationVisitor()(implicit fragment: Fragment) extends VerificationStmtVisitor {
  override def visitXary: PartialFunction[(StmtNode, List[Compilation[Unit]]), Compilation[Unit]] = {
    case (node: TopLevelExprNode.ContinuationNode, _) => Compilation.fail(IllegalContinuation(node))
    case args => super.visitXary(args)
  }
}

object NoContinuationVisitor {
  case class IllegalContinuation(node: TopLevelExprNode.ContinuationNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"A continuation is only valid as the very last top-level expression of a constructor block."
  }
}
