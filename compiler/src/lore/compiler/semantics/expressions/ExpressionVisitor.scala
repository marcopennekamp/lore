package lore.compiler.semantics.expressions

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.semantics.expressions.Expression._

trait ExpressionVisitor[A, B] {
  type Result = B

  // Top-Level Expressions
  def visit(expression: Return)(value: A): B
  def visit(expression: VariableDeclaration)(value: A): B
  def visit(expression: Assignment)(target: A, value: A): B

  // Expressions
  def visit(expression: Block)(expressions: Vector[A]): B
  def visit(expression: VariableAccess): B
  def visit(expression: MemberAccess)(instance: A): B
  def visit(expression: UnresolvedMemberAccess)(instance: A): B = throw CompilationException("UnresolvedMemberAccess is not supported by this visitor.")
  def visit(expression: Literal): B
  def visit(expression: Tuple)(values: Vector[A]): B
  def visit(expression: AnonymousFunction)(body: A): B
  def visit(expression: MultiFunctionValue): B
  def visit(expression: FixedFunctionValue): B
  def visit(expression: ListConstruction)(values: Vector[A]): B
  def visit(expression: MapConstruction)(entries: Vector[(A, A)]): B
  def visit(expression: ShapeValue)(properties: Vector[A]): B
  def visit(expression: Symbol): B
  def visit(expression: Instantiation)(arguments: Vector[A]): B
  def visit(expression: UnaryOperation)(value: A): B
  def visit(expression: BinaryOperation)(left: A, right: A): B
  def visit(expression: XaryOperation)(operands: Vector[A]): B
  def visit(expression: Call)(target: Option[A], arguments: Vector[A]): B
  def visit(expression: IfElse)(condition: A, onTrue: A, onFalse: A): B
  def visit(expression: WhileLoop)(condition: A, body: A): B
  def visit(expression: ForLoop)(collections: Vector[A], body: A): B

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
      case node@Return(value, _) => visitor.visit(node)(rec(value))
      case node@VariableDeclaration(_, value, _) => visitor.visit(node)(rec(value))
      case node@Assignment(target, value, _) => visitor.visit(node)(rec(target), rec(value))

      // Expressions
      case node@Block(expressions, _) => visitor.visit(node)(expressions.map(rec))
      case node@VariableAccess(_, _) => visitor.visit(node)
      case node@MemberAccess(instance, _, _) => visitor.visit(node)(rec(instance))
      case node@UnresolvedMemberAccess(instance, _, _, _) => visitor.visit(node)(rec(instance))
      case node@Literal(_, _, _) => visitor.visit(node)
      case node@Tuple(values, _) => visitor.visit(node)(values.map(rec))
      case node@AnonymousFunction(_, body, _) => visitor.visit(node)(rec(body))
      case node@MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@FixedFunctionValue(_, _) => visitor.visit(node)
      case node@ListConstruction(values, _, _) => visitor.visit(node)(values.map(rec))
      case node@MapConstruction(entries, _, _) => visitor.visit(node)(entries.map(e => (rec(e.key), rec(e.value))))
      case node@Instantiation(_, arguments, _) => visitor.visit(node)(arguments.map(arg => rec(arg.value)))
      case node@ShapeValue(properties, _) => visitor.visit(node)(properties.map(p => rec(p.value)))
      case node@Symbol(_, _) => visitor.visit(node)
      case node@UnaryOperation(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@BinaryOperation(_, left, right, _, _) => visitor.visit(node)(rec(left), rec(right))
      case node@XaryOperation(_, expressions, _, _) => visitor.visit(node)(expressions.map(rec))
      case node@Call(target, arguments, _, _) => visitor.visit(node)(target.getExpression.map(rec), arguments.map(rec))
      case node@IfElse(condition, onTrue, onFalse, _, _) => visitor.visit(node)(rec(condition), rec(onTrue), rec(onFalse))
      case node@WhileLoop(condition, body, _, _) => visitor.visit(node)(rec(condition), rec(body))
      case node@ForLoop(extractors, body, _, _) => visitor.visit(node)(extractors.map(e => rec(e.collection)), rec(body))
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
      case node@Return(value, _) => rec(value).flatMap(visitor.visit(node))
      case node@VariableDeclaration(_, value, _) => rec(value).flatMap(visitor.visit(node))
      case node@Assignment(target, value, _) => (rec(target), rec(value)).simultaneous.flatMap((visitor.visit(node) _).tupled)

      // Expressions
      case node@Block(expressions, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@VariableAccess(_, _) => visitor.visit(node)
      case node@MemberAccess(instance, _, _) => rec(instance).flatMap(visitor.visit(node))
      case node@UnresolvedMemberAccess(instance, _, _, _) => rec(instance).flatMap(visitor.visit(node))
      case node@Literal(_, _, _) => visitor.visit(node)
      case node@Tuple(values, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@AnonymousFunction(_, body, _) => rec(body).flatMap(visitor.visit(node))
      case node@MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@FixedFunctionValue(_, _) => visitor.visit(node)
      case node@ListConstruction(values, _, _) => values.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@MapConstruction(entries, _, _) => entries.map(e => (rec(e.key), rec(e.value)).simultaneous).simultaneous.flatMap(visitor.visit(node))
      case node@ShapeValue(properties, _) => properties.map(p => rec(p.value)).simultaneous.flatMap(visitor.visit(node))
      case node@Symbol(_, _) => visitor.visit(node)
      case node@Instantiation(_, arguments, _) => arguments.map(_.value).map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@UnaryOperation(_, value, _, _) => rec(value).flatMap(visitor.visit(node))
      case node@BinaryOperation(_, left, right, _, _) => (rec(left), rec(right)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@XaryOperation(_, expressions, _, _) => expressions.map(rec).simultaneous.flatMap(visitor.visit(node))
      case node@Call(target, arguments, _, _) => (target.getExpression.map(rec).toCompiledOption, arguments.map(rec).simultaneous).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@IfElse(condition, onTrue, onFalse, _, _) =>
        (rec(condition), rec(onTrue), rec(onFalse)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@WhileLoop(condition, body, _, _) => (rec(condition), rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
      case node@ForLoop(extractors, body, _, _) =>
        (extractors.map(e => rec(e.collection)).simultaneous, rec(body)).simultaneous.flatMap((visitor.visit(node) _).tupled)
    }
  }
}
