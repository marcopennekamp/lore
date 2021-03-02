package lore.compiler.semantics.expressions

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.semantics.expressions.Expression._

/**
  * By default, this visitor copies expressions as-is. You can override individual methods to modify individual
  * expression nodes.
  *
  * Note that the legality of expression types will not be checked by this visitor. It's the responsibility of the
  * implementor to produce well typed expressions.
  */
trait ExpressionIdentityVisitor[R] extends ExpressionVisitor[Expression, R] {

  protected def wrap(expression: Expression): R

  override def visit(expression: Return)(value: Expression): R = wrap(expression.copy(value))
  override def visit(expression: VariableDeclaration)(value: Expression): R = wrap(expression.copy(value = value))
  override def visit(expression: Assignment)(target: Expression, value: Expression): R = wrap(expression.copy(target.asInstanceOf[Access], value))
  override def visit(expression: Block)(expressions: Vector[Expression]): R = wrap(expression.copy(expressions))
  override def visit(expression: VariableAccess): R = wrap(expression)
  override def visit(expression: MemberAccess)(instance: Expression): R = wrap(expression.copy(instance))
  override def visit(expression: Literal): R = wrap(expression)
  override def visit(expression: Tuple)(values: Vector[Expression]): R = wrap(expression.copy(values))
  override def visit(expression: ListConstruction)(values: Vector[Expression]): R = wrap(expression.copy(values))
  override def visit(expression: MapConstruction)(entries: Vector[(Expression, Expression)]): R = wrap(expression.withEntries(entries))
  override def visit(expression: ShapeValue)(properties: Vector[Expression]): R = wrap(expression.withPropertyValues(properties))
  override def visit(expression: Instantiation)(arguments: Vector[Expression]): R = wrap(expression.withArgumentValues(arguments))
  override def visit(expression: UnaryOperation)(value: Expression): R = wrap(expression.copy(value = value))
  override def visit(expression: BinaryOperation)(left: Expression, right: Expression): R = wrap(expression.copy(left = left, right = right))
  override def visit(expression: XaryOperation)(operands: Vector[Expression]): R = wrap(expression.copy(expressions = operands))
  override def visit(expression: Call)(arguments: Vector[Expression]): R = wrap(expression.copy(arguments = arguments))
  override def visit(expression: IfElse)(condition: Expression, onTrue: Expression, onFalse: Expression): R = wrap(expression.copy(condition, onTrue, onFalse))
  override def visit(expression: WhileLoop)(condition: Expression, body: Expression): R = wrap(expression.copy(condition, body))
  override def visit(expression: ForLoop)(collections: Vector[Expression], body: Expression): R = wrap(expression.withCollections(collections).copy(body = body))

}

object ExpressionIdentityVisitor {

  trait Simple extends ExpressionIdentityVisitor[Expression] {
    override def wrap(expression: Expression): Expression = expression
  }

  trait Compiled extends ExpressionIdentityVisitor[Compilation[Expression]] {
    override def wrap(expression: Expression): Compilation[Expression] = expression.compiled
  }

}
