package lore.compiler.semantics.expressions

import lore.compiler.core.Position
import lore.compiler.phases.verification.LocalVariable
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.{ProductType, Type}

/**
  * Keeping the distinction between TopLevelExpression and Expression even up to this level grants us a little bit
  * of safety still when building the expression tree from the abstract syntax tree.
  */
trait TopLevelExpression {
  def position: Position
  def tpe: Type
}

object TopLevelExpression {
  abstract class Apply(override val tpe: Type) extends TopLevelExpression

  case class Return(value: Expression, position: Position) extends TopLevelExpression.Apply(ProductType.UnitType)

  case class VariableDeclaration(
    variable: LocalVariable, value: Option[Expression], position: Position,
  ) extends TopLevelExpression.Apply(ProductType.UnitType)

  case class Assignment(
    target: Expression.Access, value: Expression, position: Position,
  ) extends TopLevelExpression.Apply(ProductType.UnitType)

  // TODO: Can ConstructorCall be represented by FunctionCall?
  case class Construct(
    definition: ClassDefinition, arguments: List[Expression], withSuper: Option[Expression.Call],
    position: Position,
  ) extends TopLevelExpression.Apply(definition.tpe)
}
