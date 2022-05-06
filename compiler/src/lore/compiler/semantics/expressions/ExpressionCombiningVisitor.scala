package lore.compiler.semantics.expressions

trait ExpressionCombiningVisitor[A, B] extends ExpressionVisitor[A, B] {
  def combine(values: Vector[A]): B

  private def combine(values: A*): B = combine(values.toVector)

  private def combinePairs(values: Vector[(A, A)]): B = {
    combine(values.flatMap { case (a, b) => Vector(a, b) })
  }

  override def visit(expression: Expression.Return)(value: A): B = combine(value)
  override def visit(expression: Expression.VariableDeclaration)(value: A): B = combine(value)
  override def visit(expression: Expression.Assignment)(target: A, value: A): B = combine(target, value)
  override def visit(expression: Expression.Block)(expressions: Vector[A]): B = combine(expressions)
  override def visit(expression: Expression.BindingAccess): B = combine()
  override def visit(expression: Expression.MemberAccess)(instance: A): B = combine(instance)
  override def visit(expression: Expression.Literal): B = combine()
  override def visit(expression: Expression.Tuple)(values: Vector[A]): B = combine(values)
  override def visit(expression: Expression.AnonymousFunction)(body: A): B = combine(body)
  override def visit(expression: Expression.MultiFunctionValue): B = combine()
  override def visit(expression: Expression.FixedFunctionValue): B = combine()
  override def visit(expression: Expression.ConstructorValue): B = combine()
  override def visit(expression: Expression.ListConstruction)(values: Vector[A]): B = combine(values)
  override def visit(expression: Expression.MapConstruction)(entries: Vector[(A, A)]): B = combinePairs(entries)
  override def visit(expression: Expression.ShapeValue)(properties: Vector[A]): B = combine(properties)
  override def visit(expression: Expression.PropertyDefaultValue): B = combine()
  override def visit(expression: Expression.UnaryOperation)(value: A): B = combine(value)
  override def visit(expression: Expression.BinaryOperation)(left: A, right: A): B = combine(left, right)
  override def visit(expression: Expression.XaryOperation)(operands: Vector[A]): B = combine(operands)
  override def visit(expression: Expression.Call)(target: Option[A], arguments: Vector[A]): B = combine(target.toVector ++ arguments)
  override def visit(expression: Expression.Cond)(cases: Vector[(A, A)]): B = combinePairs(cases)
  override def visit(expression: Expression.WhileLoop)(condition: A, body: A): B = combine(condition, body)
  override def visit(expression: Expression.ForLoop)(collections: Vector[A], body: A): B = combine(collections :+ body)
  override def visit(expression: Expression.Ascription)(value: A): B = combine(value)
}
