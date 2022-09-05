package lore.compiler.semantics.expressions.typed

import lore.compiler.semantics.expressions.typed.Expression._

trait ExpressionVerificationVisitor extends ExpressionVisitor[Unit, Unit] {
  /**
    * Verifies any given expression. The default implementation succeeds.
    */
  def verify(expression: Expression): Result = ()

  override def visit(expression: Hole): Result = verify(expression)
  override def visit(expression: TypeAscription)(value: Unit): Result = verify(expression)
  override def visit(expression: IntValue): Result = verify(expression)
  override def visit(expression: RealValue): Result = verify(expression)
  override def visit(expression: BooleanValue): Result = verify(expression)
  override def visit(expression: StringValue): Result = verify(expression)
  override def visit(expression: SymbolValue): Result = verify(expression)
  override def visit(expression: TupleValue)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: LambdaValue)(body: Unit): Result = verify(expression)
  override def visit(expression: MultiFunctionValue): Result = verify(expression)
  override def visit(expression: FixedFunctionValue): Result = verify(expression)
  override def visit(expression: ConstructorValue): Result = verify(expression)
  override def visit(expression: ListValue)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: ShapeValue)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: PropertyDefaultValue): Unit = verify(expression)
  override def visit(expression: UnaryOperation)(value: Unit): Result = verify(expression)
  override def visit(expression: BinaryOperation)(left: Unit, right: Unit): Result = verify(expression)
  override def visit(expression: XaryOperation)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: MultiFunctionCall)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: ConstructorCall)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: ValueCall)(target: Unit, arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: IntrinsicCall)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: VariableDeclaration)(value: Unit): Result = verify(expression)
  override def visit(expression: Assignment)(target: Unit, value: Unit): Result = verify(expression)
  override def visit(expression: BindingAccess): Result = verify(expression)
  override def visit(expression: MemberAccess)(instance: Unit): Result = verify(expression)
  override def visit(expression: Return)(value: Unit): Result = verify(expression)
  override def visit(expression: Block)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Cond)(cases: Vector[(Unit, Unit)]): Unit = verify(expression)
  override def visit(expression: WhileLoop)(condition: Unit, body: Unit): Result = verify(expression)
  override def visit(expression: ForLoop)(collections: Vector[Unit], body: Unit): Result = verify(expression)
}
