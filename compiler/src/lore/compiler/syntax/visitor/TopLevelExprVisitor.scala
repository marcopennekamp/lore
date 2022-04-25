package lore.compiler.syntax.visitor

import lore.compiler.syntax.{ExprNode, TopLevelExprNode}
import scalaz.Id.Id

/**
  * Visits any top-level expression node, returning a value of type B. Subtrees are visited automatically. Subtree
  * compilations are aggregated and flat-mapped into higher nodes.
  *
  * Functions named before* are invoked BEFORE subtrees are visited. Functions named visit* are invoked AFTER.
  */
trait TopLevelExprVisitor[A, M[_]] {
  /**
    * Visits a node in the AST without a child.
    */
  def visitLeaf(node: TopLevelExprNode.LeafNode): M[A]

  /**
    * Visits a node with exactly one child.
    */
  def visitUnary(node: TopLevelExprNode.UnaryNode)(argument: A): M[A]

  /**
    * Visits a node with exactly two children.
    */
  def visitBinary(node: TopLevelExprNode.BinaryNode)(left: A, right: A): M[A]

  /**
    * Visits a node with exactly three children.
    */
  def visitTernary(node: TopLevelExprNode.TernaryNode)(argument1: A, argument2: A, argument3: A): M[A]

  /**
    * Visits a node with exactly one list of children.
    */
  def visitXary(node: TopLevelExprNode.XaryNode)(arguments: Vector[A]): M[A]

  /**
    * Visits an anonymous function node with its body.
    *
    * Similar to [[visitIteration]] in that we're passing control to the visitor about when to visit the body.
    */
  def visitAnonymousFunction(node: ExprNode.AnonymousFunctionNode)(visitBody: () => M[A]): M[A]

  /**
    * Visits a map node with its key/value entries.
    */
  def visitMap(node: ExprNode.MapNode)(entries: Vector[(A, A)]): M[A]

  /**
    * Visits a call node with its target and its arguments.
    */
  def visitCall(node: ExprNode.CallNode)(target: A, arguments: Vector[A]): M[A]

  /**
    * Visits a cond node with its cases.
    */
  def visitCond(node: ExprNode.CondNode)(cases: Vector[(A, A)]): M[A]

  /**
    * Visits an iteration node with its extractors and the body.
    *
    * We don't pass the evaluated extractors and body but rather a function that visits these AST nodes. This allows
    * the iteration visitor to properly implement scoping and process the extractors before the body is visited.
    */
  def visitIteration(node: ExprNode.ForNode)(visitExtractors: Vector[() => M[A]], visitBody: () => M[A]): M[A]

  /**
    * Invoked before a node's subtrees are visited. This can be used to set up variable declarations and more.
    */
  def before: PartialFunction[TopLevelExprNode, M[Unit]] = PartialFunction.empty
}

object TopLevelExprVisitor {

  def visit[A](visitor: TopLevelExprVisitor[A, Id])(node: TopLevelExprNode): A = new Applicator[A, Unit](visitor).visit(node, ())

  class Applicator[A, Props](visitor: TopLevelExprVisitor[A, Id]) {
    /**
      * Visits the whole tree invoking begin* and visit* functions for every node.
      */
    final def visit(node: TopLevelExprNode, props: Props): A = {
      visitor.before.applyOrElse(node, (_: TopLevelExprNode) => ())
      handleMatch(node, props)
    }

    protected def handleMatch(node: TopLevelExprNode, props: Props): A = {
      import ExprNode._
      import TopLevelExprNode._

      node match {
        case node: LeafNode => visitor.visitLeaf(node)
        case node: UnaryNode => visitor.visitUnary(node)(visit(node.child, props))
        case node: BinaryNode => visitor.visitBinary(node)(visit(node.child1, props), visit(node.child2, props))
        case node: TernaryNode => visitor.visitTernary(node)(visit(node.child1, props), visit(node.child2, props), visit(node.child3, props))
        case node: XaryNode => visitor.visitXary(node)(node.children.map(visit(_, props)))

        case node@AnonymousFunctionNode(_, body, _) => visitor.visitAnonymousFunction(node)(() => visit(body, props))

        case node@MapNode(entries, _) =>
          val visitedEntries = entries.map {
            case KeyValueNode(key, value, _) => (visit(key, props), visit(value, props))
          }
          visitor.visitMap(node)(visitedEntries)

        case node@CallNode(target, arguments, _) => visitor.visitCall(node)(visit(target, props), arguments.map(visit(_, props)))

        case node@CondNode(cases, _) =>
          val visitedCases = cases.map {
            case CondCaseNode(condition, body, _) => (visit(condition, props), visit(body, props))
          }
          visitor.visitCond(node)(visitedCases)

        case node@ForNode(extractors, body, _) =>
          val visitExtractors = extractors.map {
            case ExtractorNode(_, collection, _) => () => visit(collection, props)
          }
          visitor.visitIteration(node)(visitExtractors, () => visit(body, props))
      }
    }
  }

}
