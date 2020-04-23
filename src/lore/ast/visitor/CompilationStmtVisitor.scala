package lore.ast.visitor

import lore.ast.{ExprNode, StmtNode}
import lore.compiler.Compilation

/**
  * Represents a statement visitor resulting in a Compilation. This trait implements default "pass on results and
  * errors" (also called "smile and nod") behavior for all nodes. You can then extend the partial functions exactly
  * how you want to implement functionality only for the nodes that you need to process.
  *
  * By default, all compilations are aggregated using simultaneous.
  */
trait CompilationStmtVisitor[A] extends StmtVisitor[Compilation[A]] {
  def empty: A
  def combine(list: List[A]): A
  private def combine(tuple: (A, A)): A = combine(List(tuple._1, tuple._2))
  private def combine(tuple: (A, A, A)): A = combine(List(tuple._1, tuple._2, tuple._3))

  override def visitLeaf: PartialFunction[StmtNode, Compilation[A]] = {
    case _ => Compilation.succeed(empty)
  }
  override def visitUnary: PartialFunction[(StmtNode, Compilation[A]), Compilation[A]] = {
    case (_, comp) => comp
  }
  override def visitBinary: PartialFunction[(StmtNode, (Compilation[A], Compilation[A])), Compilation[A]] = {
    case (_, comps) => comps.simultaneous.map(combine)
  }
  override def visitTernary: PartialFunction[(StmtNode, (Compilation[A], Compilation[A], Compilation[A])), Compilation[A]] = {
    case (_, comps) => comps.simultaneous.map(combine)
  }
  override def visitXary: PartialFunction[(StmtNode, List[Compilation[A]]), Compilation[A]] = {
    case (_, comps) => comps.simultaneous.map(combine)
  }
  override def visitMap(node: ExprNode.MapNode, entries: List[(Compilation[A], Compilation[A])]): Compilation[A] = {
    // This might seem overly complex, but it only aggregates all the compilation results for the map entries.
    entries.map(_.simultaneous).simultaneous.map { entries => entries.map(combine) }.map(combine)
  }
  override def visitIteration(node: ExprNode.IterationNode, extractors: List[(String, Compilation[A])], body: Compilation[A]): Compilation[A] = {
    (
      extractors.map(_._2).simultaneous.map(combine),
      body
    ).simultaneous.map(combine)
  }
}
