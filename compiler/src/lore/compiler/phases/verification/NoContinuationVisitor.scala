package lore.compiler.phases.verification

import lore.compiler.ast.visitor.VerificationStmtVisitor
import lore.compiler.ast.{StmtNode, TopLevelExprNode}
import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.phases.verification.NoContinuationVisitor.IllegalContinuation

class NoContinuationVisitor extends VerificationStmtVisitor {
  override def verify(node: StmtNode): Verification = node match {
    case node: TopLevelExprNode.ContinuationNode => Compilation.fail(IllegalContinuation(node))
    case _ => Verification.succeed
  }
}

object NoContinuationVisitor {
  case class IllegalContinuation(node: TopLevelExprNode.ContinuationNode) extends Error(node) {
    override def message = s"A continuation is only valid as the very last top-level expression of a constructor block."
  }
}
