package lore.ast

import lore.types.Type

/**
  * All statements and expressions.
  */
sealed trait StmtNode
object StmtNode {
  case class ReturnNode(expr: ExprNode) extends StmtNode
}

/**
  * Expressions that are only allowed at the top-level, i.e. as immediate children of blocks or as a body
  * of conditionals and repetitions.
  *
  * This concept is used to implement the restriction that assignments can only stand in specific places.
  */
sealed trait TopLevelExprNode extends StmtNode {
  /**
    * The result type of the expression. If None, the type has not been inferred yet. This property is meant to
    * cache the result of type inference and is set by the type inference mechanism.
    */
  var resultType: Option[Type] = None
}

object TopLevelExprNode {
  case class VariableDeclarationNode(name: String, value: ExprNode, isMutable: Boolean) extends TopLevelExprNode
  case class AssignmentNode(address: AddressNode, value: ExprNode) extends TopLevelExprNode

  sealed trait AddressNode
  case class VariableAddressNode(name: String) extends AddressNode
  case class PropertyAddressNode(address: AddressNode, propertyName: String) extends AddressNode
}

/**
  * Expressions returning some kind of value. Almost every statement in Lore is an expression.
  */
sealed trait ExprNode extends TopLevelExprNode
object ExprNode {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double) extends ExprNode
  case class IntLiteralNode(value: Int) extends ExprNode
  case class AdditionNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class NegationNode(expr: ExprNode) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean) extends ExprNode
  case class ConjunctionNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class DisjunctionNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class LogicalNotNode(expr: ExprNode) extends ExprNode
  case class EqualsNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class LessThanNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class GreaterThanNode(left: ExprNode, right: ExprNode) extends ExprNode
  case class GreaterThanEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String) extends ExprNode
  case class ConcatenationNode(exprs: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(exprs: List[ExprNode]) extends ExprNode

  /**
    * The unit tuple.
    */
  case object UnitNode extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(exprs: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode]) extends ExprNode
  case class KeyValueNode(key: String, value: ExprNode)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class InstantiationNode(typeName: String, constructorName: Option[String], arguments: List[ExprNode]) extends ExprNode
  case class PropertyAccessNode(instance: ExprNode, propertyName: String) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(statements: List[StmtNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MultiFunctionCallNode(name: String, arguments: List[ExprNode]) extends ExprNode
  case class FixedFunctionCallNode(name: String, types: List[TypeExprNode], arguments: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and repetition expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(condition: ExprNode, onTrue: TopLevelExprNode, onFalse: TopLevelExprNode) extends ExprNode

  /**
    * @param deferCheck Whether the condition should be checked after the loop body.
    */
  case class RepeatWhileNode(condition: ExprNode, body: TopLevelExprNode, deferCheck: Boolean) extends ExprNode

  case class IterationNode(extractions: List[ExtractionNode], body: TopLevelExprNode) extends ExprNode
  case class ExtractionNode(variableName: String, collection: ExprNode)
  case class YieldNode(expr: ExprNode) extends ExprNode
}
