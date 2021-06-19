package lore.compiler.semantics.expressions

import lore.compiler.core.{Compilation, CompilationException}

trait ExpressionVisitor[A, B] {
  type Result = B

  // Top-Level Expressions
  def visit(expression: Expression.Return)(value: A): B
  def visit(expression: Expression.VariableDeclaration)(value: A): B
  def visit(expression: Expression.Assignment)(target: A, value: A): B

  // Expressions
  def visit(expression: Expression.Block)(expressions: Vector[A]): B
  def visit(expression: Expression.VariableAccess): B
  def visit(expression: Expression.MemberAccess)(instance: A): B
  def visit(expression: Expression.UnresolvedMemberAccess)(instance: A): B = throw CompilationException("UnresolvedMemberAccess is not supported by this visitor.")
  def visit(expression: Expression.Literal): B
  def visit(expression: Expression.Tuple)(values: Vector[A]): B
  def visit(expression: Expression.AnonymousFunction)(body: A): B
  def visit(expression: Expression.MultiFunctionValue): B
  def visit(expression: Expression.FixedFunctionValue): B
  def visit(expression: Expression.ListConstruction)(values: Vector[A]): B
  def visit(expression: Expression.MapConstruction)(entries: Vector[(A, A)]): B
  def visit(expression: Expression.ShapeValue)(properties: Vector[A]): B
  def visit(expression: Expression.Instantiation)(arguments: Vector[A]): B
  def visit(expression: Expression.UnaryOperation)(value: A): B
  def visit(expression: Expression.BinaryOperation)(left: A, right: A): B
  def visit(expression: Expression.XaryOperation)(operands: Vector[A]): B
  def visit(expression: Expression.Call)(target: Option[A], arguments: Vector[A]): B
  def visit(expression: Expression.IfElse)(condition: A, onTrue: A, onFalse: A): B
  def visit(expression: Expression.WhileLoop)(condition: A, body: A): B
  def visit(expression: Expression.ForLoop)(collections: Vector[A], body: A): B

  /**
    * Invoked before an expressions's subtrees are visited. This can be used to set up contexts.
    */
  def before: PartialFunction[Expression, Unit] = PartialFunction.empty
}

object ExpressionVisitor {
  /**
    * Visits the whole tree invoking before and visit* functions for every expression. Does not handle Compilations.
    */
  final def visit[A](visitor: ExpressionVisitor[A, A])(expression: Expression): A = {
    val rec = visit(visitor) _
    visitor.before.applyOrElse(expression, (_: Expression) => ())
    expression match {
      // Top-Level Expressions
      case node@Expression.Return(value, _) => visitor.visit(node)(rec(value))
      case node@Expression.VariableDeclaration(_, value, _) => visitor.visit(node)(rec(value))
      case node@Expression.Assignment(target, value, _) => visitor.visit(node)(rec(target), rec(value))

      // Expressions
      case node@Expression.Block(expressions, _) => visitor.visit(node)(expressions.map(rec))
      case node@Expression.VariableAccess(_, _) => visitor.visit(node)
      case node@Expression.MemberAccess(instance, _, _) => visitor.visit(node)(rec(instance))
      case node@Expression.UnresolvedMemberAccess(instance, _, _, _) => visitor.visit(node)(rec(instance))
      case node@Expression.Literal(_, _, _) => visitor.visit(node)
      case node@Expression.Tuple(values, _) => visitor.visit(node)(values.map(rec))
      case node@Expression.AnonymousFunction(_, body, _) => visitor.visit(node)(rec(body))
      case node@Expression.MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@Expression.FixedFunctionValue(_, _) => visitor.visit(node)
      case node@Expression.ListConstruction(values, _, _) => visitor.visit(node)(values.map(rec))
      case node@Expression.MapConstruction(entries, _, _) => visitor.visit(node)(entries.map(e => (rec(e.key), rec(e.value))))
      case node@Expression.Instantiation(_, arguments, _) => visitor.visit(node)(arguments.map(arg => rec(arg.value)))
      case node@Expression.ShapeValue(properties, _) => visitor.visit(node)(properties.map(p => rec(p.value)))
      case node@Expression.UnaryOperation(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@Expression.BinaryOperation(_, left, right, _, _) => visitor.visit(node)(rec(left), rec(right))
      case node@Expression.XaryOperation(_, expressions, _, _) => visitor.visit(node)(expressions.map(rec))
      case node@Expression.Call(target, arguments, _, _) => visitor.visit(node)(target.getExpression.map(rec), arguments.map(rec))
      case node@Expression.IfElse(condition, onTrue, onFalse, _, _) => visitor.visit(node)(rec(condition), rec(onTrue), rec(onFalse))
      case node@Expression.WhileLoop(condition, body, _, _) => visitor.visit(node)(rec(condition), rec(body))
      case node@Expression.ForLoop(extractors, body, _, _) => visitor.visit(node)(extractors.map(e => rec(e.collection)), rec(body))
    }
  }

  /**
    * Visits the whole tree invoking before and visit* functions for every expression. Handles Compilations natively.
    */
  final def visitCompilation[A](visitor: ExpressionVisitor[A, Compilation[A]])(expression: Expression): Compilation[A] = {
    val rec = visitCompilation(visitor) _
    visitor.before.applyOrElse(expression, (_: Expression) => ())
    expression match {
      // Top-Level Expressions
      case node@Expression.Return(value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.VariableDeclaration(_, value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.Assignment(target, value, _) => (rec(target), rec(value)).simultaneous.flatMap((visitor.visit(node) _).tupled)

      // Expressions
      case node@Expression.Block(expressions, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.VariableAccess(_, _) => visitor.visit(node)
      case node@Expression.MemberAccess(instance, _, _) => rec(instance).flatMap(visitor.visit(node))
      case node@Expression.UnresolvedMemberAccess(instance, _, _, _) => rec(instance).flatMap(visitor.visit(node))
      case node@Expression.Literal(_, _, _) => visitor.visit(node)
      case node@Expression.Tuple(values, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.AnonymousFunction(_, body, _) => rec(body).flatMap(visitor.visit(node))
      case node@Expression.MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@Expression.FixedFunctionValue(_, _) => visitor.visit(node)
      case node@Expression.ListConstruction(values, _, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.MapConstruction(entries, _, _) => entries.map(e => (rec(e.key), rec(e.value)).simultaneous).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.ShapeValue(properties, _) => properties.map(p => rec(p.value)).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.Instantiation(_, arguments, _) => arguments.map(_.value).map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.UnaryOperation(_, value, _, _) => rec(value).flatMap(visitor.visit(node))
      case node@Expression.BinaryOperation(_, left, right, _, _) => (rec(left), rec(right)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.XaryOperation(_, expressions, _, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Expression.Call(target, arguments, _, _) => (target.getExpression.map(rec).toCompiledOption, arguments.map(rec).simultaneous).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.IfElse(condition, onTrue, onFalse, _, _) =>
        (rec(condition), rec(onTrue), rec(onFalse)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.WhileLoop(condition, body, _, _) => (rec(condition), rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@Expression.ForLoop(extractors, body, _, _) =>
        (extractors.map(e => rec(e.collection)).simultaneous, rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
    }
  }
}
