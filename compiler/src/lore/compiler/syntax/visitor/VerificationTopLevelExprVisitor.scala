package lore.compiler.syntax.visitor

import lore.compiler.core.Compilation.Verification
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}

/**
  * A verification visitor which allows easier definition of visitor functions due to its automatic
  * discarding of compilation results (which are always Unit for a Verification).
  */
trait VerificationTopLevelExprVisitor extends TopLevelExprVisitor[Unit] {
  /**
    * Verifies any given node. The default implementation succeeds.
    */
  def verify(node: TopLevelExprNode): Verification = Verification.succeed

  override final def visitLeaf(node: TopLevelExprNode.LeafNode): Verification = verify(node)
  override final def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: Unit): Verification = verify(node)
  override final def visitBinary(node: TopLevelExprNode.BinaryNode)(left: Unit, right: Unit): Verification = verify(node)
  override final def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: Unit, argument2: Unit, argument3: Unit): Verification = verify(node)
  override final def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[Unit]): Verification = verify(node)
  override final def visitMap(node: ExprNode.MapNode)(entries: Vector[(Unit, Unit)]): Verification = verify(node)
  override def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, Unit)], visitBody: () => Verification): Verification = verify(node)
}
