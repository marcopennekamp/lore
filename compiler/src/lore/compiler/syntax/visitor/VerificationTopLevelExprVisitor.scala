package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import scalaz.Id.Id

/**
  * A verification visitor which allows easier definition through a single `verify` function.
  */
trait VerificationTopLevelExprVisitor extends TopLevelExprVisitor[Unit, Id] {
  /**
    * Verifies any given node. The default implementation simply succeeds.
    */
  def verify(node: TopLevelExprNode): Unit = ()

  override final def visitLeaf(node: TopLevelExprNode.LeafNode): Unit = verify(node)
  override final def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: Unit): Unit = verify(node)
  override final def visitBinary(node: TopLevelExprNode.BinaryNode)(left: Unit, right: Unit): Unit = verify(node)
  override final def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: Unit, argument2: Unit, argument3: Unit): Unit = verify(node)
  override final def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[Unit]): Unit = verify(node)
  override final def visitAnonymousFunction(node: ExprNode.AnonymousFunctionNode)(visitBody: () => Unit): Unit = {
    visitBody()
    verify(node)
  }
  override final def visitMap(node: ExprNode.MapNode)(entries: Vector[(Unit, Unit)]): Unit = verify(node)
  override final def visitCall(node: ExprNode.CallNode)(target: Unit, arguments: Vector[Unit]): Unit = verify(node)
  override def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, Unit)], visitBody: () => Unit): Unit = {
    visitBody()
    verify(node)
  }
}
