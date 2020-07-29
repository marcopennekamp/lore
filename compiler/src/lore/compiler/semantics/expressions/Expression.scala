package lore.compiler.semantics.expressions

import lore.compiler.core.Position
import lore.compiler.phases.verification.{LocalVariable, VirtualMember}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.{ProductType, Type}

trait Expression extends TopLevelExpression

object Expression {
  abstract class Apply(override val tpe: Type) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Block(expressions: List[TopLevelExpression], position: Position) extends Expression {
    override val tpe: Type = expressions.lastOption.map(_.tpe).getOrElse(ProductType.UnitType)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Access expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A cross-cutting node trait signifying the possible access of a variable/property and thus the target
    * of an assignment.
    */
  trait Access

  case class VariableAccess(variable: LocalVariable, position: Position) extends Expression.Apply(variable.tpe) with Access
  case class MemberAccess(instance: Expression, member: VirtualMember, position: Position) extends Expression.Apply(member.tpe) with Access

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Literals and Value Constructors.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: In Scala 3, just use a sum type instead of Any. :)
  case class Literal(value: Any, tpe: Type, position: Position) extends Expression
  case class Tuple(values: List[Expression], position: Position) extends Expression {
    override val tpe: Type = if (values.isEmpty) ProductType.UnitType else ProductType(values.map(_.tpe))
  }
  case class ListConstruction(values: List[Expression], tpe: Type, position: Position) extends Expression
  case class MapConstruction(entries: List[MapEntry], tpe: Type, position: Position) extends Expression
  case class MapEntry(key: Expression, value: Expression)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait UnaryOperator
  object UnaryOperator {
    case object Negation extends UnaryOperator
    case object LogicalNot extends UnaryOperator
  }

  sealed trait BinaryOperator
  object BinaryOperator {
    case object Addition extends BinaryOperator
    case object Subtraction extends BinaryOperator
    case object Multiplication extends BinaryOperator
    case object Division extends BinaryOperator
    case object Equals extends BinaryOperator
    case object LessThan extends BinaryOperator
    case object LessThanEquals extends BinaryOperator
  }

  sealed trait XaryOperator
  object XaryOperator {
    case object Conjunction extends XaryOperator
    case object Disjunction extends XaryOperator
    case object Concatenation extends XaryOperator
  }

  case class UnaryOperation(operator: UnaryOperator, value: Expression, tpe: Type, position: Position) extends Expression
  case class BinaryOperation(operator: BinaryOperator, left: Expression, right: Expression, tpe: Type, position: Position) extends Expression
  case class XaryOperation(operator: XaryOperator, expressions: List[Expression], tpe: Type, position: Position) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function/constructor/dynamic calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Call(target: CallTarget, arguments: List[Expression], position: Position) extends Expression.Apply(target.outputType)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class IfElse(condition: Expression, onTrue: TopLevelExpression, onFalse: TopLevelExpression, tpe: Type, position: Position) extends Expression

  trait Loop extends Expression {
    def body: TopLevelExpression
  }
  case class WhileLoop(condition: Expression, body: TopLevelExpression, tpe: Type, position: Position) extends Loop
  case class ForLoop(extractors: List[Extractor], body: TopLevelExpression, tpe: Type, position: Position) extends Loop
  case class Extractor(variable: LocalVariable, collection: Expression)
}
