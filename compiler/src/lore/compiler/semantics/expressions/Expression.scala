package lore.compiler.semantics.expressions

import lore.compiler.core.Position
import lore.compiler.semantics.{LocalVariable, VirtualMember}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.{BasicType, ProductType, Type}

sealed trait Expression {
  def position: Position
  def tpe: Type
}

object Expression {
  abstract class Apply(override val tpe: Type) extends Expression

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Top-level expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Return(value: Expression, position: Position) extends Expression.Apply(ProductType.UnitType)

  case class VariableDeclaration(
    variable: LocalVariable, value: Expression, position: Position,
  ) extends Expression.Apply(ProductType.UnitType)

  case class Assignment(
    target: Expression.Access, value: Expression, position: Position,
  ) extends Expression.Apply(ProductType.UnitType)

  case class Construct(
    definition: ClassDefinition, arguments: List[Expression], withSuper: Option[Expression],
    position: Position,
  ) extends Expression.Apply(definition.tpe)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Block(expressions: List[Expression], position: Position) extends Expression {
    override val tpe: Type = expressions.lastOption.map(_.tpe).getOrElse(ProductType.UnitType)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Access expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A cross-cutting node trait signifying the possible access of a variable/property and thus the target
    * of an assignment.
    */
  sealed trait Access extends Expression {
    def name: String
  }

  case class VariableAccess(variable: LocalVariable, position: Position) extends Expression.Apply(variable.tpe) with Access {
    override val name: String = variable.name
  }
  case class MemberAccess(instance: Expression, member: VirtualMember, position: Position) extends Expression.Apply(member.tpe) with Access {
    override val name: String = member.name
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Literals and Value Constructors.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: In Scala 3, just use a sum type instead of Any. :)
  case class Literal(value: Any, tpe: BasicType, position: Position) extends Expression
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
  case class IfElse(condition: Expression, onTrue: Expression, onFalse: Expression, tpe: Type, position: Position) extends Expression

  trait Loop extends Expression {
    def body: Expression
  }
  case class WhileLoop(condition: Expression, body: Expression, tpe: Type, position: Position) extends Loop
  case class ForLoop(extractors: List[Extractor], body: Expression, tpe: Type, position: Position) extends Loop
  case class Extractor(variable: LocalVariable, collection: Expression)
}
