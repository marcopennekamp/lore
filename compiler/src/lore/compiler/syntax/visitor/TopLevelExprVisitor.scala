package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import lore.compiler.core.Compilation

/**
  * Visits any top-level expression node, returning a value of type Compilation[A]. Subtrees are visited automatically.
  * Subtree compilations are aggregated and flat-mapped into higher nodes.
  *
  * Functions named before* are invoked BEFORE subtrees are visited. Functions named visit* are invoked AFTER.
  */
trait TopLevelExprVisitor[A] {
  /**
    * Visits a node in the AST without a child.
    */
  def visitLeaf(node: TopLevelExprNode.LeafNode): Compilation[A]

  /**
    * Visits a node with exactly one child.
    */
  def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: A): Compilation[A]

  /**
    * Visits a node with exactly two children.
    */
  def visitBinary(node: TopLevelExprNode.BinaryNode)(left: A, right: A): Compilation[A]

  /**
    * Visits a node with exactly three children.
    */
  def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: A, argument2: A, argument3: A): Compilation[A]

  /**
    * Visits a node with exactly one list of children.
    */
  def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[A]): Compilation[A]

  /**
    * Visits a map node with its key/value entries.
    */
  def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): Compilation[A]

  /**
    * Visits an iteration node with its extractors and the body.
    *
    * We don't pass the evaluated body but rather a function that visits the body expression. This allows the
    * iteration visitor to process the extractors before the body is visited.
    */
  def visitIteration(node: ExprNode.ForNode)(extractors: Vector[(String, A)], visitBody: () => Compilation[A]): Compilation[A]

  /**
    * Invoked before a node's subtrees are visited. This can be used to set up contexts and such.
    */
  def before: PartialFunction[TopLevelExprNode, Unit] = PartialFunction.empty
}

object TopLevelExprVisitor {
  class Applicator[A, Props](visitor: TopLevelExprVisitor[A]) {
    /**
      * Visits the whole tree invoking begin* and visit* functions for every node.
      */
    final def visit(node: TopLevelExprNode, props: Props): Compilation[A] = {
      // Apply the before* callback before we visit the node's subtrees.
      visitor.before.applyOrElse(node, (_: TopLevelExprNode) => ())
      handleMatch(node, props)
    }

    protected def handleMatch(node: TopLevelExprNode, props: Props): Compilation[A] = {
      // Importing all expression nodes makes the code far easier to digest visually.
      import TopLevelExprNode._
      import ExprNode._

      node match {
        case node: LeafNode => visitor.visitLeaf(node)
        case node: UnaryNode => visit(node.child, props).flatMap(visitor.visitUnary(node))
        case node: BinaryNode => (visit(node.child1, props), visit(node.child2, props)).simultaneous.flatMap((visitor.visitBinary(node) _).tupled)
        case node: TernaryNode => (visit(node.child1, props), visit(node.child2, props), visit(node.child3, props)).simultaneous.flatMap((visitor.visitTernary(node) _).tupled)
        case node: XaryNode => node.children.map(c => visit(c, props)).simultaneous.flatMap(visitor.visitXary(node))

        // Map node.
        case node@MapNode(kvs, _) =>
          val entries = kvs.map {
            case KeyValueNode(key, value, _) => (visit(key, props), visit(value, props)).simultaneous
          }.simultaneous
          entries.flatMap(visitor.visitMap(node))

        // Iteration node.
        case node@ForNode(extractors, body, _) =>
          val extracts = extractors.map {
            case ExtractorNode(name, collection, _) => visit(collection, props).map((name, _))
          }.simultaneous
          extracts.flatMap(extractors => visitor.visitIteration(node)(extractors, () => visit(body, props)))
      }
    }
  }

  def visit[A](visitor: TopLevelExprVisitor[A])(node: TopLevelExprNode): Compilation[A] = new Applicator[A, Unit](visitor).visit(node, ())
}
