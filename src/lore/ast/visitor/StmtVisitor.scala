package lore.ast.visitor

import lore.ast.StmtNode.{BinaryNode, UnaryNode, XaryNode}
import lore.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.Compilation

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
  /**
    * Visits the whole tree invoking begin* and visit* functions for every node.
    */
  def visit[A](visitor: StmtVisitor[A])(node: StmtNode): Compilation[A] = {
    // A shorthand for visiting subtrees.
    val rec = visit(visitor) _

    // Some helper methods to make the following definitions easier.
    def unary(node: UnaryNode, expr: StmtNode): Compilation[A] = {
      rec(expr).flatMap(visitor.visitUnary(node))
    }

    def binary(node: BinaryNode, left: StmtNode, right: StmtNode): Compilation[A] = {
      (rec(left), rec(right)).simultaneous.flatMap((visitor.visitBinary(node) _).tupled)
    }

    def xary(node: XaryNode, exprs: List[StmtNode]): Compilation[A] = {
      exprs.map(rec).simultaneous.flatMap(visitor.visitXary(node))
    }

    // Apply the before* callback before we visit the node's subtrees.
    visitor.before.applyOrElse(node, (_: StmtNode) => ())

    // Importing all statement nodes makes the code far easier to digest visually.
    import ExprNode._
    import StmtNode._
    import TopLevelExprNode._

    node match {
      // Leafs.
      case node@VariableNode(_)                           => visitor.visitLeaf(node)
      case node@RealLiteralNode(_)                        => visitor.visitLeaf(node)
      case node@IntLiteralNode(_)                         => visitor.visitLeaf(node)
      case node@BoolLiteralNode(_)                        => visitor.visitLeaf(node)
      case node@StringLiteralNode(_)                      => visitor.visitLeaf(node)
      case UnitNode                                       => visitor.visitLeaf(UnitNode)

      // Unary.
      case node@ReturnNode(expr)                          => unary(node, expr)
      case node@VariableDeclarationNode(_, _, _, value)   => unary(node, value)
      case node@YieldNode(expr)                           => unary(node, expr)
      case node@NegationNode(expr)                        => unary(node, expr)
      case node@LogicalNotNode(expr)                      => unary(node, expr)
      case node@PropertyAccessNode(instance, _)           => unary(node, instance)

      // Binary.
      case node@AssignmentNode(address, value)            => binary(node, address, value)
      case node@AdditionNode(left, right)                 => binary(node, left, right)
      case node@SubtractionNode(left, right)              => binary(node, left, right)
      case node@MultiplicationNode(left, right)           => binary(node, left, right)
      case node@DivisionNode(left, right)                 => binary(node, left, right)
      case node@EqualsNode(left, right)                   => binary(node, left, right)
      case node@NotEqualsNode(left, right)                => binary(node, left, right)
      case node@LessThanNode(left, right)                 => binary(node, left, right)
      case node@LessThanEqualsNode(left, right)           => binary(node, left, right)
      case node@GreaterThanNode(left, right)              => binary(node, left, right)
      case node@GreaterThanEqualsNode(left, right)        => binary(node, left, right)
      case node@RepeatWhileNode(condition, body, _)       => binary(node, condition, body)

      // Ternary.
      case node@IfElseNode(condition, onTrue, onFalse) =>
        (rec(condition), rec(onTrue), rec(onFalse)).simultaneous.flatMap((visitor.visitTernary(node) _).tupled)

      // Xary.
      case node@ConstructNode(arguments, _)               => xary(node, arguments)
      case node@ConstructorCallNode(_, arguments)         => xary(node, arguments)
      case node@ConjunctionNode(expressions)              => xary(node, expressions)
      case node@DisjunctionNode(expressions)              => xary(node, expressions)
      case node@ConcatenationNode(expressions)            => xary(node, expressions)
      case node@TupleNode(expressions)                    => xary(node, expressions)
      case node@ListNode(expressions)                     => xary(node, expressions)
      case node@BlockNode(statements)                     => xary(node, statements)
      case node@SimpleCallNode(_, _, arguments)           => xary(node, arguments)
      case node@FixedFunctionCallNode(_, _, arguments)    => xary(node, arguments)

      // Map node.
      case node@MapNode(kvs) =>
        val entries = kvs.map {
          case ExprNode.KeyValueNode(key, value) => (rec(key), rec(value)).simultaneous
        }.simultaneous
        entries.flatMap(visitor.visitMap(node))

      // Iteration node.
      case node@IterationNode(extractors, body) =>
        val extracts = extractors.map {
          case ExprNode.ExtractorNode(name, collection) => rec(collection).map((name, _))
        }.simultaneous
        extracts.flatMap(extractors => visitor.visitIteration(node)(extractors, () => rec(body)))
    }
  }
}
