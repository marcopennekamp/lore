package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.Compilation
import lore.compiler.syntax.TopLevelExprNode.ConstructNode

/**
  * Visits any statement node, returning a value of type Compilation[A]. Subtrees are visited automatically.
  * Subtree compilations are aggregated and flat-mapped into higher nodes.
  *
  * Functions named before* are invoked BEFORE subtrees are visited. Functions named visit* are invoked AFTER.
  */
trait StmtVisitor[A] {
  /**
    * Visits a node in the AST without a child.
    */
  def visitLeaf(node: StmtNode.LeafNode): Compilation[A]

  /**
    * Visits a node with exactly one child.
    */
  def visitUnary(node: StmtNode.UnaryNode)(argument: A): Compilation[A]

  /**
    * Visits a node with exactly two children.
    */
  def visitBinary(node: StmtNode.BinaryNode)(left: A, right: A): Compilation[A]

  /**
    * Visits a node with exactly three children.
    */
  def visitTernary(node: StmtNode.TernaryNode)(argument1: A, argument2: A, argument3: A): Compilation[A]

  /**
    * Visits a node with exactly one list of children.
    */
  def visitXary(node: StmtNode.XaryNode)(arguments: List[A]): Compilation[A]

  /**
    * Visits a construct node with its arguments and super call.
    */
  def visitConstruct(node: TopLevelExprNode.ConstructNode)(arguments: List[A], withSuper: Option[A]): Compilation[A]

  /**
    * Visits a map node with its key/value entries.
    */
  def visitMap(node: ExprNode.MapNode)(entries: List[(A, A)]): Compilation[A]

  /**
    * Visits an iteration node with its extractors and the body.
    *
    * We don't pass the evaluated body but rather a function that visits the body expression. This allows the
    * iteration visitor to process the extractors before the body is visited.
    */
  def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, A)], visitBody: () => Compilation[A]): Compilation[A]

  /**
    * Invoked before a node's subtrees are visited. This can be used to set up contexts and such.
    */
  def before: PartialFunction[StmtNode, Unit] = PartialFunction.empty
}

object StmtVisitor {
  class Applicator[A, Props](visitor: StmtVisitor[A]) {
    /**
      * Visits the whole tree invoking begin* and visit* functions for every node.
      */
    final def visit(node: StmtNode, props: Props): Compilation[A] = {
      // Apply the before* callback before we visit the node's subtrees.
      visitor.before.applyOrElse(node, (_: StmtNode) => ())
      handleMatch(node, props)
    }

    protected def handleMatch(node: StmtNode, props: Props): Compilation[A] = {
      // Importing all statement nodes makes the code far easier to digest visually.
      import ExprNode._
      import StmtNode._

      node match {
        case node: LeafNode => visitor.visitLeaf(node)
        case node: UnaryNode => visit(node.child, props).flatMap(visitor.visitUnary(node))
        case node: BinaryNode => (visit(node.child1, props), visit(node.child2, props)).simultaneous.flatMap((visitor.visitBinary(node) _).tupled)
        case node: TernaryNode => (visit(node.child1, props), visit(node.child2, props), visit(node.child3, props)).simultaneous.flatMap((visitor.visitTernary(node) _).tupled)
        case node: XaryNode => node.children.map(c => visit(c, props)).simultaneous.flatMap(visitor.visitXary(node))

        // Construct node.
        case node@ConstructNode(arguments, withSuper, _) =>
          (
            arguments.map(a => visit(a, props)).simultaneous,
            withSuper.map(c => visit(c, props)).toCompiledOption
          ).simultaneous.flatMap((visitor.visitConstruct(node) _).tupled)

        // Map node.
        case node@MapNode(kvs, _) =>
          val entries = kvs.map {
            case KeyValueNode(key, value, _) => (visit(key, props), visit(value, props)).simultaneous
          }.simultaneous
          entries.flatMap(visitor.visitMap(node))

        // Iteration node.
        case node@IterationNode(extractors, body, _) =>
          val extracts = extractors.map {
            case ExtractorNode(name, collection, _) => visit(collection, props).map((name, _))
          }.simultaneous
          extracts.flatMap(extractors => visitor.visitIteration(node)(extractors, () => visit(body, props)))
      }
    }
  }

  def visit[A](visitor: StmtVisitor[A])(node: StmtNode): Compilation[A] = new Applicator[A, Unit](visitor).visit(node, ())
}
