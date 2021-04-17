package lore.compiler.semantics.expressions

import lore.compiler.core.Compilation.Verification

trait ExpressionVerificationVisitor extends ExpressionVisitor[Unit, Verification] {
  /**
    * Verifies any given expression. The default implementation succeeds.
    */
  def verify(expression: Expression): Verification = Verification.succeed
  
  override def visit(expression: Expression.Return)(value: Unit): Result = verify(expression)
  override def visit(expression: Expression.VariableDeclaration)(value: Unit): Result = verify(expression)
  override def visit(expression: Expression.Assignment)(target: Unit, value: Unit): Result = verify(expression)
  override def visit(expression: Expression.Block)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.VariableAccess): Result = verify(expression)
  override def visit(expression: Expression.MemberAccess)(instance: Unit): Result = verify(expression)
  override def visit(expression: Expression.UnresolvedMemberAccess)(instance: Unit): Result = verify(expression)
  override def visit(expression: Expression.Literal): Result = verify(expression)
  override def visit(expression: Expression.Tuple)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.AnonymousFunction)(body: Unit): Verification = verify(expression)
  override def visit(expression: Expression.MultiFunctionValue): Verification = verify(expression)
  override def visit(expression: Expression.ListConstruction)(values: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.MapConstruction)(entries: Vector[(Unit, Unit)]): Result = verify(expression)
  override def visit(expression: Expression.Instantiation)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.ShapeValue)(arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.UnaryOperation)(value: Unit): Result = verify(expression)
  override def visit(expression: Expression.BinaryOperation)(left: Unit, right: Unit): Result = verify(expression)
  override def visit(expression: Expression.XaryOperation)(expressions: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.Call)(target: Option[Unit], arguments: Vector[Unit]): Result = verify(expression)
  override def visit(expression: Expression.IfElse)(condition: Unit, onTrue: Unit, onFalse: Unit): Result = verify(expression)
  override def visit(expression: Expression.WhileLoop)(condition: Unit, body: Unit): Result = verify(expression)
  override def visit(expression: Expression.ForLoop)(collections: Vector[Unit], body: Unit): Result = verify(expression)
}
