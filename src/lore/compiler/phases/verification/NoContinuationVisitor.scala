package lore.compiler.phases.verification

import lore.ast.{StmtNode, TopLevelExprNode}
import lore.ast.visitor.VerificationStmtVisitor
import lore.compiler.{Compilation, Fragment}
import lore.compiler.feedback.Error

class NoContinuationVisitor()(implicit fragment: Fragment) extends VerificationStmtVisitor {
  override def visitXary: PartialFunction[(StmtNode, List[Compilation[Unit]]), Compilation[Unit]] = {
    case (node: TopLevelExprNode.ContinuationNode, _) => Compilation.fail(Error.IllegalContinuation(node))
    case args => super.visitXary(args)
  }
}
