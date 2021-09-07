package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, AssignmentsExtension}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression._
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.members.Member
import lore.compiler.semantics.scopes.{TypedBinding, Variable}
import lore.compiler.semantics.structures.StructConstructor
import lore.compiler.types._

/**
  * Replaces all inference variables with inferred types.
  */
class TypeRehydrationVisitor(assignments: Assignments)(implicit registry: Registry, reporter: Reporter) extends ExpressionVisitor[Expression, Expression] {

  override def visit(expression: Hole): Expression = expression.copy(assignments.instantiate(expression.tpe))

  override def visit(expression: Return)(value: Expression): Expression = expression.copy(value)

  override def visit(expression: VariableDeclaration)(value: Expression): Expression = expression.copy(
    variable = instantiateVariable(expression.variable),
    value = value
  )

  override def visit(expression: Assignment)(target: Expression, value: Expression): Expression = expression.copy(
    target = target.asInstanceOf[Expression.Access],
    value = value
  )

  override def visit(expression: Block)(expressions: Vector[Expression]): Expression = expression.copy(expressions)

  override def visit(expression: BindingAccess): Expression = expression.copy(
    binding = instantiateBinding(expression.binding)
  )

  override def visit(expression: MemberAccess)(instance: Expression): Expression = expression.copy(instance)

  override def visit(expression: UnresolvedMemberAccess)(instance: Expression): Expression = {
    val member = instance.tpe.member(expression.name) match {
      case Some(member) => member
      case None =>
        // The instance does not have a member with the given name, but type inference already uncovered this and
        // produced an appropriate error. We can simply return a dummy member as a default value so that the
        // transformation phase can continue.
        Member(expression.name, BasicType.Nothing, isAssignable = true, isMutable = true)
    }
    MemberAccess(instance, member, expression.position)
  }

  override def visit(expression: Literal): Expression = expression

  override def visit(expression: Tuple)(values: Vector[Expression]): Expression = expression.copy(values)

  override def visit(expression: AnonymousFunction)(body: Expression): Expression = expression.copy(
    expression.parameters.map(_.mapType(assignments.instantiate)),
    body
  )

  override def visit(expression: MultiFunctionValue): Expression = expression.copy(tpe = assignments.instantiate(expression.tpe))

  override def visit(expression: FixedFunctionValue): Expression = expression

  override def visit(expression: ListConstruction)(values: Vector[Expression]): Expression = expression.copy(
    values = values,
    tpe = assignments.instantiate(expression.tpe)
  )

  override def visit(expression: MapConstruction)(entries: Vector[(Expression, Expression)]): Expression = {
    expression.withEntries(entries).copy(
      tpe = assignments.instantiate(expression.tpe)
    )
  }

  override def visit(expression: ShapeValue)(properties: Vector[Expression]): Expression = expression.withPropertyValues(properties)

  override def visit(expression: Symbol): Expression = expression

  override def visit(expression: UnaryOperation)(value: Expression): Expression = expression.copy(
    value = value,
    tpe = assignments.instantiate(expression.tpe)
  )

  override def visit(expression: BinaryOperation)(left: Expression, right: Expression): Expression = expression.copy(
    left = left,
    right = right,
    tpe = assignments.instantiate(expression.tpe)
  )

  override def visit(expression: XaryOperation)(operands: Vector[Expression]): Expression = expression.copy(
    expressions = operands,
    tpe = assignments.instantiate(expression.tpe)
  )

  override def visit(expression: Call)(target: Option[Expression], arguments: Vector[Expression]): Expression = expression.copy(
    target = expression.target.withExpression(target),
    arguments = arguments,
    tpe = assignments.instantiate(expression.tpe),
  )

  override def visit(expression: IfElse)(condition: Expression, onTrue: Expression, onFalse: Expression): Expression = expression.copy(
    condition = condition,
    onTrue = onTrue,
    onFalse = onFalse,
    tpe = assignments.instantiate(expression.tpe)
  )

  override def visit(expression: Cond)(cases: Vector[(Expression, Expression)]): Expression = {
    expression.withCases(cases).copy(
      tpe = assignments.instantiate(expression.tpe)
    )
  }

  override def visit(expression: WhileLoop)(condition: Expression, body: Expression): Expression = expression.copy(
    condition = condition,
    body = body,
    tpe = instantiateLoopResultType(expression.tpe)
  )

  override def visit(expression: ForLoop)(collections: Vector[Expression], body: Expression): Expression = {
    val newLoop = expression.withCollections(collections)
    newLoop.copy(
      extractors = newLoop.extractors.map(extractor => extractor.copy(instantiateVariable(extractor.variable))),
      body = body,
      tpe = instantiateLoopResultType(expression.tpe)
    )
  }

  private def instantiateBinding(binding: TypedBinding): TypedBinding = binding match {
    case variable: Variable => instantiateVariable(variable)
    case constructor: StructConstructor => StructConstructor(assignments.instantiate(constructor.structType).asInstanceOf[StructType])
    case v => v
  }

  private def instantiateVariable(variable: Variable): Variable = variable.copy(tpe = assignments.instantiate(variable.tpe))

  /**
    * Instantiates the result type of the loop, simplifying [Unit] to Unit to cover "no result" loops.
    */
  private def instantiateLoopResultType(resultType: Type): Type = assignments.instantiate(resultType) match {
    case ListType(TupleType.UnitType) => TupleType.UnitType
    case t => t
  }

}
