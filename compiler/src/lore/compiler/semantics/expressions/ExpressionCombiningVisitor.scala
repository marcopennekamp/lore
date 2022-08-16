package lore.compiler.semantics.expressions

import lore.compiler.semantics.expressions.Expression._

trait ExpressionCombiningVisitor[A, B] extends ExpressionVisitor[A, B] {
  def combine(values: Vector[A]): B

  private def combine(values: A*): B = combine(values.toVector)

  private def combinePairs(values: Vector[(A, A)]): B = {
    combine(values.flatMap { case (a, b) => Vector(a, b) })
  }

  override def visit(expression: IntValue): B = combine()
  override def visit(expression: RealValue): B = combine()
  override def visit(expression: BooleanValue): B = combine()
  override def visit(expression: StringValue): B = combine()
  override def visit(expression: SymbolValue): B = combine()
  override def visit(expression: TupleValue)(elements: Vector[A]): B = combine(elements)
  override def visit(expression: LambdaValue)(body: A): B = combine(body)
  override def visit(expression: MultiFunctionValue): B = combine()
  override def visit(expression: FixedFunctionValue): B = combine()
  override def visit(expression: ConstructorValue): B = combine()
  override def visit(expression: ListValue)(elements: Vector[A]): B = combine(elements)
  override def visit(expression: ShapeValue)(properties: Vector[A]): B = combine(properties)
  override def visit(expression: PropertyDefaultValue): B = combine()

  override def visit(expression: UnaryOperation)(value: A): B = combine(value)
  override def visit(expression: BinaryOperation)(left: A, right: A): B = combine(left, right)
  override def visit(expression: XaryOperation)(operands: Vector[A]): B = combine(operands)
  override def visit(expression: MultiFunctionCall)(arguments: Vector[A]): B = combine(arguments)
  override def visit(expression: ValueCall)(target: A, arguments: Vector[A]): B = combine(target +: arguments)
  override def visit(expression: ConstructorCall)(arguments: Vector[A]): B = combine(arguments)
  override def visit(expression: IntrinsicCall)(arguments: Vector[A]): B = combine(arguments)

  override def visit(expression: VariableDeclaration)(value: A): B = combine(value)
  override def visit(expression: Assignment)(target: A, value: A): B = combine(target, value)
  override def visit(expression: BindingAccess): B = combine()
  override def visit(expression: MemberAccess)(instance: A): B = combine(instance)

  override def visit(expression: Return)(value: A): B = combine(value)
  override def visit(expression: Block)(expressions: Vector[A]): B = combine(expressions)
  override def visit(expression: Cond)(cases: Vector[(A, A)]): B = combinePairs(cases)
  override def visit(expression: WhileLoop)(condition: A, body: A): B = combine(condition, body)
  override def visit(expression: ForLoop)(collections: Vector[A], body: A): B = combine(collections :+ body)
}
