package lore.compiler.syntax.visitor

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.syntax.{ExprNode, TopLevelExprNode}

/**
  * Represents a top-level expression visitor that, by default, combines the results of the subtrees with some notion
  * of a combine function. You can then extend the visitor functions only for the nodes that you need to process. The
  * combine function must work for the empty list.
  */
trait CombiningTopLevelExprVisitor[A, B] extends TopLevelExprVisitor[A, B] {
  protected def combine(list: Vector[A]): A
  protected def wrap(a: A): B
  protected def flatMap(b: B, f: A => B): B

  protected def visit(node: TopLevelExprNode, results: Vector[A]): B = wrap(combine(results))

  override def visitLeaf(node: TopLevelExprNode.LeafNode): B = visit(node, Vector.empty)
  override def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: A): B = visit(node, Vector(argument))
  override def visitBinary(node: TopLevelExprNode.BinaryNode)(left: A, right: A): B = visit(node, Vector(left, right))
  override def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: A, argument2: A, argument3: A): B = {
    visit(node, Vector(argument1, argument2, argument3))
  }
  override def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[A]): B = {
    visit(node, arguments)
  }
  override def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): B = {
    visit(node, entries.map { case (a, b) => combine(Vector(a, b)) })
  }
  override def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, A)], visitBody: () => B): B = {
    flatMap(visitBody(), body => visit(node, extractors.map(_._2) ++ Vector(body)))
  }
}

object CombiningTopLevelExprVisitor {
  trait Identity[A] extends CombiningTopLevelExprVisitor[A, A] {
    override protected def wrap(a: A): A = a
    override protected def flatMap(b: A, f: A => A): A = f(b)
  }

  trait WithCompilation[A] extends CombiningTopLevelExprVisitor[A, Compilation[A]] {
    override protected def wrap(a: A): Compilation[A] = a.compiled
    override protected def flatMap(b: Compilation[A], f: A => Compilation[A]): Compilation[A] = b.flatMap(f)
  }
}
