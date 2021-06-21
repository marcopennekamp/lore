package lore.compiler.semantics.expressions

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.expressions.Expression._

trait ExpressionVerificationVisitor extends ExpressionVisitor[Unit, Verification] {
  /**
    * Verifies any given expression. The default implementation succeeds.
    */
  def verify(expression: Expression): Verification = Verification.succeed
  
  override def visit(expression: Return)(value: Unit): Result = verify(expression)
  override def visit(expression: VariableDeclaration)(value: Unit): Result = verify(expression)
  override def visit(expression: Assignment)(target: Unit, value: Unit): Result = verify(expression)
  override def visit(expression: Block)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: VariableAccess): Result = verify(expression)
  override def visit(expression: MemberAccess)(instance: Unit): Result = verify(expression)
  override def visit(expression: UnresolvedMemberAccess)(instance: Unit): Result = verify(expression)
  override def visit(expression: Literal): Result = verify(expression)
  override def visit(expression: Tuple)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: AnonymousFunction)(body: Unit): Verification = verify(expression)
  override def visit(expression: MultiFunctionValue): Verification = verify(expression)
  override def visit(expression: FixedFunctionValue): Verification = verify(expression)
  override def visit(expression: ListConstruction)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: MapConstruction)(entries: Vector[(Unit, Unit)]): Result = verify(expression)
  override def visit(expression: Instantiation)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: ShapeValue)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Symbol): Result = verify(expression)
  override def visit(expression: UnaryOperation)(value: Unit): Result = verify(expression)
  override def visit(expression: BinaryOperation)(left: Unit, right: Unit): Result = verify(expression)
  override def visit(expression: XaryOperation)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Call)(target: Option[Unit], arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: IfElse)(condition: Unit, onTrue: Unit, onFalse: Unit): Result = verify(expression)
  override def visit(expression: WhileLoop)(condition: Unit, body: Unit): Result = verify(expression)
  override def visit(expression: ForLoop)(collections: Vector[Unit], body: Unit): Result = verify(expression)
}
