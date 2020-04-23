package lore.ast.visitor

import lore.ast.{ExprNode, StmtNode, TopLevelExprNode}

trait StmtVisitor[A] {
  /**
    * Visits a node in the AST without a single StmtNode child.
    */
  def visitLeaf: PartialFunction[StmtNode, A]

  /**
    * Visits a node with exactly one StmtNode child.
    */
  def visitUnary: PartialFunction[(StmtNode, A), A]

  /**
    * Visits a node with exactly two StmtNode children.
    */
  def visitBinary: PartialFunction[(StmtNode, (A, A)), A]

  /**
    * Visits a node with exactly three StmtNode children.
    */
  def visitTernary: PartialFunction[(StmtNode, (A, A, A)), A]

  /**
    * Visits a node with exactly one list of StmtNode children.
    */
  def visitXary: PartialFunction[(StmtNode, List[A]), A]

  /**
    * Visits a map node with its key/value entries.
    */
  def visitMap(node: ExprNode.MapNode, entries: List[(A, A)]): A

  /**
    * Visits an iteration node with its extractors and the body.
    */
  def visitIteration(node: ExprNode.IterationNode, extractors: List[(String, A)], body: A): A
}

object StmtVisitor {
  /**
    * Visits the whole tree invoking visitNode at
    */
  def visit[A](visitor: StmtVisitor[A])(node: StmtNode): A = {
    val rec = visit(visitor) _
    node match {
      // Statements.
      case StmtNode.ReturnNode(expr) => visitor.visitUnary((node, rec(expr)))

      // Top-level expressions.
      case TopLevelExprNode.VariableDeclarationNode(_, _, _, value) => visitor.visitUnary((node, rec(value)))
      case TopLevelExprNode.AssignmentNode(_, value) => visitor.visitUnary((node, rec(value)))
      case TopLevelExprNode.YieldNode(expr) => visitor.visitUnary((node, rec(expr)))
      case TopLevelExprNode.ConstructNode(arguments, _) => visitor.visitXary((node, arguments.map(rec)))
      case TopLevelExprNode.ConstructorCallNode(_, arguments) => visitor.visitXary((node, arguments.map(rec)))

      // Expressions.
      case ExprNode.VariableNode(_) => visitor.visitLeaf(node)
      case ExprNode.RealLiteralNode(_) => visitor.visitLeaf(node)
      case ExprNode.IntLiteralNode(_) => visitor.visitLeaf(node)
      case ExprNode.AdditionNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.SubtractionNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.MultiplicationNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.DivisionNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.NegationNode(expr) => visitor.visitUnary((node, rec(expr)))
      case ExprNode.BoolLiteralNode(_) => visitor.visitLeaf(node)
      case ExprNode.ConjunctionNode(expressions) => visitor.visitXary((node, expressions.map(rec)))
      case ExprNode.DisjunctionNode(expressions) => visitor.visitXary((node, expressions.map(rec)))
      case ExprNode.LogicalNotNode(expr) => visitor.visitUnary((node, rec(expr)))
      case ExprNode.EqualsNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.NotEqualsNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.LessThanNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.LessThanEqualsNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.GreaterThanNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.GreaterThanEqualsNode(left, right) => visitor.visitBinary((node, (rec(left), rec(right))))
      case ExprNode.StringLiteralNode(_) => visitor.visitLeaf(node)
      case ExprNode.ConcatenationNode(expressions) => visitor.visitXary((node, expressions.map(rec)))
      case ExprNode.TupleNode(expressions) => visitor.visitXary((node, expressions.map(rec)))
      case ExprNode.UnitNode => visitor.visitLeaf(node)
      case ExprNode.ListNode(expressions) => visitor.visitXary((node, expressions.map(rec)))
      case node@ExprNode.MapNode(kvs) =>
        val entries = kvs.map { case ExprNode.KeyValueNode(key, value) => (rec(key), rec(value)) }
        visitor.visitMap(node, entries)
      case ExprNode.PropertyAccessNode(instance, _) => visitor.visitUnary((node, rec(instance)))
      case ExprNode.BlockNode(statements) => visitor.visitXary((node, statements.map(rec)))
      case ExprNode.CallNode(_, _, arguments) => visitor.visitXary((node, arguments.map(rec)))
      case ExprNode.FixedFunctionCallNode(_, _, arguments) => visitor.visitXary((node, arguments.map(rec)))
      case ExprNode.IfElseNode(condition, onTrue, onFalse) => visitor.visitTernary((node, (rec(condition), rec(onTrue), rec(onFalse))))
      case ExprNode.RepeatWhileNode(condition, body, _) => visitor.visitBinary((node, (rec(condition), rec(body))))
      case node@ExprNode.IterationNode(extractors, body) =>
        val extracts = extractors.map { case ExprNode.ExtractorNode(name, collection) => (name, rec(collection)) }
        visitor.visitIteration(node, extracts, rec(body))
    }
  }
}
