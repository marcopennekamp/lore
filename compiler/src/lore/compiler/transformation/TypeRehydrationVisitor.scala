package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.{LocalVariable, TypedTermBinding}
import lore.compiler.semantics.expressions.Expression._
import lore.compiler.semantics.expressions.{Expression, ExpressionIdentityVisitor}
import lore.compiler.semantics.members.Member
import lore.compiler.types._
import lore.compiler.typing.InferenceVariable.Assignments

// TODO (multi-import): This will be rolled into type checking when UntypedExpressions are transformed to Expressions.

/**
  * Replaces all inference variables with inferred types.
  */
class TypeRehydrationVisitor(assignments: Assignments)(
  implicit registry: Registry,
  reporter: Reporter,
) extends ExpressionIdentityVisitor[Expression] {

  override protected def wrap(expression: Expression): Expression = expression

  override def visit(expression: Hole): Expression = expression.copy(assignments.instantiate(expression.tpe))

  override def visit(expression: VariableDeclaration)(value: Expression): Expression = expression.copy(
    variable = instantiateVariable(expression.variable),
    value = value
  )

  override def visit(expression: Block)(expressions: Vector[Expression]): Expression = {
    // For a block expression expected to result in Unit, we have to manually add a unit value if the block's last
    // expression doesn't already result in Unit.
    val blockType = assignments.instantiate(expression.tpe)
    val allExpressions = if (blockType == TupleType.UnitType && expressions.last.tpe != TupleType.UnitType) {
      expressions :+ Expression.Tuple(Vector.empty, expressions.last.position)
    } else expressions
    expression.copy(expressions = allExpressions, tpe = blockType)
  }

  override def visit(expression: BindingAccess): Expression = expression.copy(
    binding = instantiateBinding(expression.binding)
  )

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

  override def visit(expression: AnonymousFunction)(body: Expression): Expression = expression.copy(
    expression.parameters.map(_.mapType(assignments.instantiate)),
    body
  )

  override def visit(expression: MultiFunctionValue): Expression = {
    expression.copy(tpe = assignments.instantiate(expression.tpe))
  }

  override def visit(expression: UntypedConstructorValue): Expression = {
    assignments.instantiate(expression.tpe) match {
      case FunctionType(_, structType: StructType) => ConstructorValue(expression.binding, structType, expression.position)
      case _ =>
        // If the instantiated type is not a function type with a struct output type, we can be sure that typechecking
        // will have produced an appropriate error. Hence we don't need to report anything here.
        Expression.Hole(BasicType.Nothing, expression.position)
    }
  }

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

  override def visit(expression: Call)(target: Option[Expression], arguments: Vector[Expression]): Expression = {
    expression.copy(
      target = expression.target.withExpression(target),
      arguments = arguments,
      tpe = assignments.instantiate(expression.tpe),
    )
  }

  override def visit(expression: ForLoop)(collections: Vector[Expression], body: Expression): Expression = {
    val newLoop = expression.withCollections(collections)
    newLoop.copy(
      extractors = newLoop.extractors.map(extractor => extractor.copy(instantiateVariable(extractor.variable))),
      body = body,
    )
  }

  private def instantiateBinding(binding: TypedTermBinding): TypedTermBinding = binding match {
    case variable: LocalVariable => instantiateVariable(variable)
    case v => v
  }

  private def instantiateVariable(variable: LocalVariable): LocalVariable = {
    variable.copy(tpe = assignments.instantiate(variable.tpe))
  }

}
