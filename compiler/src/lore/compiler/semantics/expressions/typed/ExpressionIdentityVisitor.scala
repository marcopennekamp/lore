package lore.compiler.semantics.expressions.typed

import lore.compiler.semantics.expressions.typed.Expression._

/**
  * By default, this visitor copies expressions as-is. You can override individual methods to modify individual
  * expression nodes.
  *
  * Note that the legality of expression types will not be checked by this visitor. It's the responsibility of the
  * implementor to produce well typed expressions.
  */
trait ExpressionIdentityVisitor[R] extends ExpressionVisitor[Expression, R] {
  protected def wrap(expression: Expression): R

  override def visit(expression: Hole): R = wrap(expression)
  override def visit(expression: TypeAscription)(value: Expression): R = wrap(expression.copy(value = value))

  override def visit(expression: IntValue): R = wrap(expression)
  override def visit(expression: RealValue): R = wrap(expression)
  override def visit(expression: BooleanValue): R = wrap(expression)
  override def visit(expression: StringValue): R = wrap(expression)
  override def visit(expression: SymbolValue): R = wrap(expression)
  override def visit(expression: TupleValue)(elements: Vector[Expression]): R = wrap(expression.copy(elements))
  override def visit(expression: LambdaValue)(body: Expression): R = wrap(expression.copy(body = body))
  override def visit(expression: MultiFunctionValue): R = wrap(expression)
  override def visit(expression: FixedFunctionValue): R = wrap(expression)
  override def visit(expression: ConstructorValue): Result = wrap(expression)
  override def visit(expression: ListValue)(elements: Vector[Expression]): R = wrap(expression.copy(elements))
  override def visit(expression: ShapeValue)(properties: Vector[Expression]): R = wrap(expression.withPropertyValues(properties))
  override def visit(expression: PropertyDefaultValue): R = wrap(expression)

  override def visit(expression: UnaryOperation)(value: Expression): R = wrap(expression.copy(value = value))
  override def visit(expression: BinaryOperation)(left: Expression, right: Expression): R = wrap(expression.copy(left = left, right = right))
  override def visit(expression: XaryOperation)(operands: Vector[Expression]): R = wrap(expression.copy(operands = operands))
  override def visit(expression: MultiFunctionCall)(arguments: Vector[Expression]): R = wrap(expression.copy(arguments = arguments))
  override def visit(expression: ConstructorCall)(arguments: Vector[Expression]): R = wrap(expression.copy(arguments = arguments))
  override def visit(expression: ValueCall)(target: Expression, arguments: Vector[Expression]): R = wrap(expression.copy(target = target, arguments = arguments))
  override def visit(expression: IntrinsicCall)(arguments: Vector[Expression]): R = wrap(expression.copy(arguments = arguments))

  override def visit(expression: VariableDeclaration)(value: Expression): R = wrap(expression.copy(value = value))
  override def visit(expression: Assignment)(target: Expression, value: Expression): R = wrap(expression.copy(target.asInstanceOf[Access], value))
  override def visit(expression: BindingAccess): R = wrap(expression)
  override def visit(expression: MemberAccess)(instance: Expression): R = wrap(expression.copy(instance))

  override def visit(expression: Return)(value: Expression): R = wrap(expression.copy(value))
  override def visit(expression: Block)(expressions: Vector[Expression]): R = wrap(expression.copy(expressions))
  override def visit(expression: Cond)(cases: Vector[(Expression, Expression)]): R = wrap(expression.withCases(cases))
  override def visit(expression: WhileLoop)(condition: Expression, body: Expression): R = wrap(expression.copy(condition, body))
  override def visit(expression: ForLoop)(collections: Vector[Expression], body: Expression): R = wrap(expression.withCollections(collections).copy(body = body))
}

object ExpressionIdentityVisitor {
  trait Simple extends ExpressionIdentityVisitor[Expression] {
    override def wrap(expression: Expression): Expression = expression
  }
}
