package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import scalaz.Id.Id

/**
  * Represents a top-level expression visitor that, by default, combines the results of the subtrees with some notion
  * of a combine function. You can then extend the visitor functions only for the nodes that you need to process. The
  * combine function must work for the empty list.
  */
trait CombiningTopLevelExprVisitor[A, M[_]] extends TopLevelExprVisitor[A, M] {
  protected def combine(list: Vector[A]): A
  protected def wrap(a: A): M[A]
  protected def flatMap(b: M[A], f: A => M[A]): M[A]

  protected def visit(node: TopLevelExprNode, results: Vector[A]): M[A] = wrap(combine(results))

  override def visitLeaf(node: TopLevelExprNode.LeafNode): M[A] = visit(node, Vector.empty)
  override def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: A): M[A] = visit(node, Vector(argument))
  override def visitBinary(node: TopLevelExprNode.BinaryNode)(left: A, right: A): M[A] = visit(node, Vector(left, right))
  override def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: A, argument2: A, argument3: A): M[A] = {
    visit(node, Vector(argument1, argument2, argument3))
  }
  override def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[A]): M[A] = {
    visit(node, arguments)
  }
  override def visitAnonymousFunction(node: ExprNode.AnonymousFunctionNode)(visitBody: () => M[A]): M[A] = {
    flatMap(visitBody(), body => visit(node, Vector(body)))
  }
  override def visitCall(node: ExprNode.CallNode)(target: A, arguments: Vector[A]): M[A] = {
    visit(node, Vector(target) ++ arguments)
  }
  override def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): M[A] = {
    visit(node, combinePairs(entries))
  }
  override def visitCond(node: ExprNode.CondNode)(cases: Vector[(A, A)]): M[A] = {
    visit(node, combinePairs(cases))
  }
  override def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, A)], visitBody: () => M[A]): M[A] = {
    flatMap(visitBody(), body => visit(node, extractors.map(_._2) ++ Vector(body)))
  }

  private def combinePairs(pairs: Vector[(A, A)]): Vector[A] = {
    pairs.map { case (a1, a2) => combine(Vector(a1, a2)) }
  }
}

object CombiningTopLevelExprVisitor {
  trait Identity[A] extends CombiningTopLevelExprVisitor[A, Id] {
    override protected def wrap(a: A): A = a
    override protected def flatMap(b: A, f: A => A): A = f(b)
  }

  trait OrVisitor extends CombiningTopLevelExprVisitor.Identity[Boolean] {
    override protected def combine(list: Vector[Boolean]): Boolean = list.exists(identity)
  }
}
