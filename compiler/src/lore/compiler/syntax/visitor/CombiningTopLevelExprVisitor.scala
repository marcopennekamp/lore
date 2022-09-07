package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import scalaz.Id.Id

/**
  * Represents a top-level expression visitor that, by default, combines the results of the subtrees with some notion
  * of a combine function. You can then extend the visitor functions only for the nodes that you need to process. The
  * combine function must work for the empty list.
  */
trait CombiningTopLevelExprVisitor[A] extends TopLevelExprVisitor[A, Id] {
  protected def combine(list: Vector[A]): A

  protected def visit(node: TopLevelExprNode, results: Vector[A]): A = combine(results)

  override def visitLeaf(node: TopLevelExprNode.LeafNode): A = visit(node, Vector.empty)
  override def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: A): A = visit(node, Vector(argument))
  override def visitBinary(node: TopLevelExprNode.BinaryNode)(left: A, right: A): A = visit(node, Vector(left, right))
  override def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: A, argument2: A, argument3: A): A = {
    visit(node, Vector(argument1, argument2, argument3))
  }
  override def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[A]): A = {
    visit(node, arguments)
  }
  override def visitLambdaValue(node: ExprNode.LambdaValueNode)(visitBody: () => A): A = {
    visit(node, Vector(visitBody()))
  }
  override def visitCall(node: ExprNode.CallNode)(target: A, arguments: Vector[A]): A = {
    visit(node, Vector(target) ++ arguments)
  }
  override def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): A = {
    visit(node, combinePairs(entries))
  }
  override def visitCond(node: ExprNode.CondNode)(cases: Vector[(A, A)]): A = {
    visit(node, combinePairs(cases))
  }
  override def visitIteration(node: ExprNode.ForNode)(visitExtractors: Vector[() => A], visitBody: () => A): A = {
    visit(node, visitExtractors.map(_.apply()) :+ visitBody())
  }

  private def combinePairs(pairs: Vector[(A, A)]): Vector[A] = {
    pairs.map { case (a1, a2) => combine(Vector(a1, a2)) }
  }
}
