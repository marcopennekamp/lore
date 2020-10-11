package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension

/**
  * Represents a statement visitor that, by default, combines the results of the subtrees with some notion of a
  * combine function. You can then extend the visitor functions only for the nodes that you need to process. The
  * combine function must work for the empty list.
  */
trait CombiningStmtVisitor[A] extends StmtVisitor[A] {
  def combine(list: Vector[A]): A

  def visit(node: StmtNode, results: Vector[A]): Compilation[A] = combine(results).compiled

  override def visitLeaf(node: StmtNode.LeafNode): Compilation[A] = visit(node, Vector.empty)
  override def visitUnary(node: StmtNode.UnaryNode)(argument: A): Compilation[A] = visit(node, Vector(argument))
  override def visitBinary(node: StmtNode.BinaryNode)(left: A, right: A): Compilation[A] = visit(node, Vector(left, right))
  override def visitTernary(node: StmtNode.TernaryNode)(argument1: A, argument2: A, argument3: A): Compilation[A] = {
    visit(node, Vector(argument1, argument2, argument3))
  }
  override def visitXary(node: StmtNode.XaryNode)(arguments: Vector[A]): Compilation[A] = {
    visit(node, arguments)
  }
  override def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): Compilation[A] = {
    visit(node, entries.map { case (a, b) => combine(Vector(a, b)) })
  }
  override def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, A)], visitBody: () => Compilation[A]): Compilation[A] = {
    visitBody().flatMap(body => visit(node, extractors.map(_._2) ++ Vector(body)))
  }
}
