package lore.compiler.phases.verification

import lore.ast.StmtNode
import lore.ast.visitor.VerificationStmtVisitor
import lore.compiler.Compilation.Verification
import lore.compiler.Fragment

private class ReturnConstraintsVisitor()(implicit fragment: Fragment) extends VerificationStmtVisitor {
  override def verify(node: StmtNode): Verification = node match {
    case _ => ???
  }
}

object ReturnConstraints {
  /**
    * Verifies the following two constraints:
    * - A return must be the last statement in the block. This effectively disallows dead code after
    *   a return statement.
    * - Constructions such as `if ({ return 0 }) a else b` are not allowed. Returning should not be
    *   possible from blocks that are in an expression position.
    */
  def verify(body: StmtNode)(implicit fragment: Fragment): Verification = {
    // TODO: Implement.
    Verification.succeed
  }
}
