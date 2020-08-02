package lore.compiler.semantics.expressions

import lore.compiler.core.Compilation

trait ExpressionVisitor[A] {
  type Result = Compilation[A]

  // Top-Level Expressions
  def visit(expression: Expression.Return)(value: A): Result
  def visit(expression: Expression.VariableDeclaration)(value: A): Result
  def visit(expression: Expression.Assignment)(value: A): Result
  def visit(expression: Expression.Construct)(arguments: List[A], superCall: Option[A]): Result

  // Expressions
  def visit(expression: Expression.Block)(expressions: List[A]): Result
  def visit(expression: Expression.VariableAccess): Result
  def visit(expression: Expression.MemberAccess)(instance: A): Result
  def visit(expression: Expression.Literal): Result
  def visit(expression: Expression.Tuple)(values: List[A]): Result
  def visit(expression: Expression.ListConstruction)(values: List[A]): Result
  def visit(expression: Expression.MapConstruction)(entries: List[(A, A)]): Result
  def visit(expression: Expression.UnaryOperation)(value: A): Result
  def visit(expression: Expression.BinaryOperation)(left: A, right: A): Result
  def visit(expression: Expression.XaryOperation)(operands: List[A]): Result
  def visit(expression: Expression.Call)(arguments: List[A]): Result
  def visit(expression: Expression.IfElse)(condition: A, onTrue: A, onFalse: A): Result
  def visit(expression: Expression.WhileLoop)(condition: A, body: A): Result
  def visit(expression: Expression.ForLoop)(extractors: List[A], body: A): Result

  /**
    * Invoked before an expressions's subtrees are visited. This can be used to set up contexts.
    */
  def before: PartialFunction[Expression, Unit] = PartialFunction.empty
}

object ExpressionVisitor {
  /**
    * Visits the whole tree invoking begin* and visit* functions for every expression.
    */
  final def visit[A](visitor: ExpressionVisitor[A])(expression: Expression): Compilation[A] = {
    val rec = visit(visitor) _
    visitor.before.applyOrElse(expression, (_: Expression) => ())
    expression match {
      // Top-Level Expressions
      case node@Expression.Return(value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.VariableDeclaration(_, value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.Assignment(_, value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.Construct(_, arguments, withSuper, _) =>
        (arguments.map(rec).simultaneous, withSuper.map(rec).toCompiledOption).simultaneous.flatMap {
          case (arguments, superCall) => visitor.visit(node)(arguments, superCall)
        }

      // Expressions
      case node@Expression.Block(expressions, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.VariableAccess(_, _) => visitor.visit(node)
      case node@Expression.MemberAccess(instance, _, _) => rec(instance).flatMap(visitor.visit(node))
      case node@Expression.Literal(_, _, _) => visitor.visit(node)
      case node@Expression.Tuple(values, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.ListConstruction(values, _, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.MapConstruction(entries, _, _) => entries.map(e => (rec(e.key), rec(e.value)).simultaneous).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.UnaryOperation(_, value, _, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.BinaryOperation(_, left, right, _, _) => (rec(left), rec(right)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.XaryOperation(_, expressions, _, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.Call(_, arguments, _) => arguments.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.IfElse(condition, onTrue, onFalse, _, _) =>
        (rec(condition), rec(onTrue), rec(onFalse)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.WhileLoop(condition, body, _, _) => (rec(condition), rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.ForLoop(extractors, body, _, _) =>
        (extractors.map(e => rec(e.collection)).simultaneous, rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
    }
  }
}
