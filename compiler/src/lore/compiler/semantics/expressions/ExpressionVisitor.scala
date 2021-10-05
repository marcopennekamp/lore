package lore.compiler.semantics.expressions

import lore.compiler.core.CompilationException
import lore.compiler.semantics.expressions.Expression._

trait ExpressionVisitor[A, B] {
  type Result = B

  // Error Recovery
  def visit(expression: Hole): B = throw CompilationException("Expression.Hole is not supported by this visitor.")

  // Top-Level Expressions
  def visit(expression: Return)(value: A): B
  def visit(expression: VariableDeclaration)(value: A): B
  def visit(expression: Assignment)(target: A, value: A): B

  // Expressions
  def visit(expression: Block)(expressions: Vector[A]): B
  def visit(expression: BindingAccess): B
  def visit(expression: MemberAccess)(instance: A): B
  def visit(expression: UnresolvedMemberAccess)(instance: A): B = throw CompilationException("Expression.UnresolvedMemberAccess is not supported by this visitor.")
  def visit(expression: Literal): B
  def visit(expression: Tuple)(values: Vector[A]): B
  def visit(expression: AnonymousFunction)(body: A): B
  def visit(expression: MultiFunctionValue): B
  def visit(expression: FixedFunctionValue): B
  def visit(expression: ListConstruction)(values: Vector[A]): B
  def visit(expression: MapConstruction)(entries: Vector[(A, A)]): B
  def visit(expression: ShapeValue)(properties: Vector[A]): B
  def visit(expression: Symbol): B
  def visit(expression: UnaryOperation)(value: A): B
  def visit(expression: BinaryOperation)(left: A, right: A): B
  def visit(expression: XaryOperation)(operands: Vector[A]): B
  def visit(expression: Call)(target: Option[A], arguments: Vector[A]): B
  def visit(expression: Cond)(cases: Vector[(A, A)]): B
  def visit(expression: WhileLoop)(condition: A, body: A): B
  def visit(expression: ForLoop)(collections: Vector[A], body: A): B

  /**
    * Invoked before an expressions's subtrees are visited. This can be used to set up contexts.
    */
  def before: PartialFunction[Expression, Unit] = PartialFunction.empty
}

object ExpressionVisitor {

  /**
    * Visits the whole tree invoking before and visit* functions for every expression.
    */
  final def visit[A](visitor: ExpressionVisitor[A, A])(expression: Expression): A = {
    val rec = visit(visitor) _
    visitor.before.applyOrElse(expression, (_: Expression) => ())
    expression match {
      // Error Recovery
      case node@Hole(_, _) => visitor.visit(node)

      // Top-Level Expressions
      case node@Return(value, _) => visitor.visit(node)(rec(value))
      case node@VariableDeclaration(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@Assignment(target, value, _) => visitor.visit(node)(rec(target), rec(value))

      // Expressions
      case node@Block(expressions, _) => visitor.visit(node)(expressions.map(rec))
      case node@BindingAccess(_, _) => visitor.visit(node)
      case node@MemberAccess(instance, _, _) => visitor.visit(node)(rec(instance))
      case node@UnresolvedMemberAccess(instance, _, _, _) => visitor.visit(node)(rec(instance))
      case node@Literal(_, _, _) => visitor.visit(node)
      case node@Tuple(values, _) => visitor.visit(node)(values.map(rec))
      case node@AnonymousFunction(_, body, _) => visitor.visit(node)(rec(body))
      case node@MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@FixedFunctionValue(_, _) => visitor.visit(node)
      case node@ListConstruction(values, _) => visitor.visit(node)(values.map(rec))
      case node@MapConstruction(entries, _) => visitor.visit(node)(entries.map(e => (rec(e.key), rec(e.value))))
      case node@ShapeValue(properties, _) => visitor.visit(node)(properties.map(p => rec(p.value)))
      case node@Symbol(_, _) => visitor.visit(node)
      case node@UnaryOperation(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@BinaryOperation(_, left, right, _, _) => visitor.visit(node)(rec(left), rec(right))
      case node@XaryOperation(_, expressions, _, _) => visitor.visit(node)(expressions.map(rec))
      case node@Call(target, arguments, _, _) => visitor.visit(node)(target.getExpression.map(rec), arguments.map(rec))
      case node@Cond(cases, _) => visitor.visit(node)(cases.map(c => (rec(c.condition), rec(c.body))))
      case node@WhileLoop(condition, body, _, _) => visitor.visit(node)(rec(condition), rec(body))
      case node@ForLoop(extractors, body, _, _) => visitor.visit(node)(extractors.map(e => rec(e.collection)), rec(body))
    }
  }

}
