package lore.compiler.semantics.expressions.typed

import lore.compiler.core.CompilationException
import lore.compiler.semantics.expressions.typed.Expression._

trait ExpressionVisitor[A, B] {
  type Result = B

  def visit(expression: Hole): B = throw CompilationException("Expression.Hole is not supported by this visitor.")
  def visit(expression: TypeAscription)(value: A): B
  // Values.
  def visit(expression: IntValue): B
  def visit(expression: RealValue): B
  def visit(expression: BooleanValue): B
  def visit(expression: StringValue): B
  def visit(expression: SymbolValue): B
  def visit(expression: TupleValue)(values: Vector[A]): B
  def visit(expression: LambdaValue)(body: A): B
  def visit(expression: MultiFunctionValue): B
  def visit(expression: FixedFunctionValue): B
  def visit(expression: ConstructorValue): B
  def visit(expression: ListValue)(values: Vector[A]): B
  def visit(expression: ShapeValue)(properties: Vector[A]): B
  def visit(expression: PropertyDefaultValue): B

  // Operators and calls.
  def visit(expression: UnaryOperation)(value: A): B
  def visit(expression: BinaryOperation)(left: A, right: A): B
  def visit(expression: XaryOperation)(operands: Vector[A]): B
  def visit(expression: MultiFunctionCall)(arguments: Vector[A]): B
  def visit(expression: ConstructorCall)(arguments: Vector[A]): B
  def visit(expression: ValueCall)(target: A, arguments: Vector[A]): B
  def visit(expression: IntrinsicCall)(arguments: Vector[A]): B

  // Variables and members.
  def visit(expression: VariableDeclaration)(value: A): B
  def visit(expression: Assignment)(target: A, value: A): B
  def visit(expression: BindingAccess): B
  def visit(expression: MemberAccess)(instance: A): B

  // Control expressions.
  def visit(expression: Return)(value: A): B
  def visit(expression: Block)(expressions: Vector[A]): B
  def visit(expression: Cond)(cases: Vector[(A, A)]): B
  def visit(expression: WhileLoop)(condition: A, body: A): B
  def visit(expression: ForLoop)(collections: Vector[A], body: A): B

  /**
    * Invoked before an expression's subtrees are visited. This can be used to set up contexts.
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
      case node@Hole(_, _) => visitor.visit(node)
      case node@TypeAscription(value, _, _) => visitor.visit(node)(rec(value))

      case node@IntValue(_, _) => visitor.visit(node)
      case node@RealValue(_, _) => visitor.visit(node)
      case node@BooleanValue(_, _) => visitor.visit(node)
      case node@StringValue(_, _) => visitor.visit(node)
      case node@SymbolValue(_, _) => visitor.visit(node)
      case node@TupleValue(elements, _) => visitor.visit(node)(elements.map(rec))
      case node@LambdaValue(_, body, _) => visitor.visit(node)(rec(body))
      case node@MultiFunctionValue(_, _, _) => visitor.visit(node)
      case node@FixedFunctionValue(_, _) => visitor.visit(node)
      case node@ConstructorValue(_, _) => visitor.visit(node)
      case node@ListValue(elements, _) => visitor.visit(node)(elements.map(rec))
      case node@ShapeValue(properties, _) => visitor.visit(node)(properties.map(p => rec(p.value)))
      case node@PropertyDefaultValue(_, _) => visitor.visit(node)

      case node@UnaryOperation(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@BinaryOperation(_, left, right, _, _) => visitor.visit(node)(rec(left), rec(right))
      case node@XaryOperation(_, operands, _, _) => visitor.visit(node)(operands.map(rec))
      case node@MultiFunctionCall(_, arguments, _) => visitor.visit(node)(arguments.map(rec))
      case node@ConstructorCall(_, arguments, _) => visitor.visit(node)(arguments.map(rec))
      case node@ValueCall(target, arguments, _, _) => visitor.visit(node)(rec(target), arguments.map(rec))
      case node@IntrinsicCall(_, arguments, _, _) => visitor.visit(node)(arguments.map(rec))

      case node@VariableDeclaration(_, value, _, _) => visitor.visit(node)(rec(value))
      case node@Assignment(target, value, _) => visitor.visit(node)(rec(target), rec(value))
      case node@BindingAccess(_, _) => visitor.visit(node)
      case node@MemberAccess(instance, _, _) => visitor.visit(node)(rec(instance))

      case node@Return(value, _) => visitor.visit(node)(rec(value))
      case node@Block(expressions, _) => visitor.visit(node)(expressions.map(rec))
      case node@Cond(cases, _) => visitor.visit(node)(cases.map(c => (rec(c.condition), rec(c.body))))
      case node@WhileLoop(condition, body, _) => visitor.visit(node)(rec(condition), rec(body))
      case node@ForLoop(extractors, body, _) => visitor.visit(node)(extractors.map(e => rec(e.collection)), rec(body))
    }
  }

}
