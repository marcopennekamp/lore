package lore.ast.visitor

import lore.ast.{ExprNode, StmtNode}
import lore.compiler.Compilation

/**
  * Represents a statement visitor resulting in a Compilation with default "pass on results and errors" (also
  * called "smile and nod") behavior for all nodes. You can then extend the partial functions exactly how you
  * want to implement functionality only for the nodes that you need to process.
  *
  * By default, all compilations are combined using simultaneous.
  */
trait PassOnStmtVisitor[A] extends StmtVisitor[A] {
  def empty: A
  def combine(list: List[A]): A
  private def combine(a1: A, a2: A): A = combine(List(a1, a2))
  private def combine(a1: A, a2: A, a3: A): A = combine(List(a1, a2, a3))

  override def visitLeaf(node: StmtNode.LeafNode): Compilation[A] = {
    Compilation.succeed(empty)
  }

  override def visitUnary(node: StmtNode.UnaryNode)(argument: A): Compilation[A] = {
    Compilation.succeed(argument)
  }

  override def visitBinary(node: StmtNode.BinaryNode)(left: A, right: A): Compilation[A] = {
    Compilation.succeed(combine(left, right))
  }

  override def visitTernary(node: StmtNode.TernaryNode)(argument1: A, argument2: A, argument3: A): Compilation[A] = {
    Compilation.succeed(combine(argument1, argument2, argument3))
  }

  override def visitXary(node: StmtNode.XaryNode)(arguments: List[A]): Compilation[A] = {
    Compilation.succeed(combine(arguments))
  }

  override def visitMap(node: ExprNode.MapNode)(entries: List[(A, A)]): Compilation[A] = {
    Compilation.succeed(combine(entries.map { case (a, b) => combine(a, b) }))
  }

  override def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, A)], visitBody: () => Compilation[A]): Compilation[A] = {
    visitBody().map(body => combine(combine(extractors.map(_._2)), body))
  }
}
