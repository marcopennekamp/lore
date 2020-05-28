package lore.compiler.ast.visitor

import lore.compiler.ast.{ExprNode, StmtNode}
import lore.compiler.core.Compilation.Verification

/**
  * A verification visitor which allows easier definition of visitor functions due to its automatic
  * discarding of compilation results (which are always Unit for a Verification).
  */
trait VerificationStmtVisitor extends StmtVisitor[Unit] {
  /**
    * Verifies any given node. The default implementation succeeds.
    */
  def verify(node: StmtNode): Verification = Verification.succeed

  override final def visitLeaf(node: StmtNode.LeafNode): Verification = verify(node)
  override final def visitUnary(node: StmtNode.UnaryNode)(argument: Unit): Verification = verify(node)
  override final def visitBinary(node: StmtNode.BinaryNode)(left: Unit, right: Unit): Verification = verify(node)
  override final def visitTernary(node: StmtNode.TernaryNode)(argument1: Unit, argument2: Unit, argument3: Unit): Verification = verify(node)
  override final def visitXary(node: StmtNode.XaryNode)(arguments: List[Unit]): Verification = verify(node)
  override final def visitMap(node: ExprNode.MapNode)(entries: List[(Unit, Unit)]): Verification = verify(node)
  override def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, Unit)], visitBody: () => Verification): Verification = verify(node)
}
