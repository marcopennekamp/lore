package lore.compiler.ast.visitor

import lore.compiler.ast.{ExprNode, StmtNode}
import lore.compiler.core.Compilation

/**
  * Represents a statement visitor that, by default, combines the results of the subtrees with some notion of a
  * combine function. You can then extend the visitor functions only for the nodes that you need to process. The
  * combine function must work for the empty list.
  */
trait CombiningStmtVisitor[A] extends StmtVisitor[A] {
  def combine(list: List[A]): A

  def visit(node: StmtNode, results: List[A]): Compilation[A] = Compilation.succeed(combine(results))

  override def visitLeaf(node: StmtNode.LeafNode): Compilation[A] = visit(node, Nil)
  override def visitUnary(node: StmtNode.UnaryNode)(argument: A): Compilation[A] = visit(node, List(argument))
  override def visitBinary(node: StmtNode.BinaryNode)(left: A, right: A): Compilation[A] = visit(node, List(left, right))
  override def visitTernary(node: StmtNode.TernaryNode)(argument1: A, argument2: A, argument3: A): Compilation[A] = {
    visit(node, List(argument1, argument2, argument3))
  }
  override def visitXary(node: StmtNode.XaryNode)(arguments: List[A]): Compilation[A] = {
    visit(node, arguments)
  }
  override def visitMap(node: ExprNode.MapNode)(entries: List[(A, A)]): Compilation[A] = {
    visit(node, entries.map { case (a, b) => combine(a :: b :: Nil) })
  }
  override def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, A)], visitBody: () => Compilation[A]): Compilation[A] = {
    visitBody().flatMap(body => visit(node, extractors.map(_._2) ::: (body :: Nil)))
  }
}
