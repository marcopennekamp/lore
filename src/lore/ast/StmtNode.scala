package lore.ast

import lore.types.Type

/**
  * All statements and expressions.
  */
sealed trait StmtNode extends Node
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
  case class VariableDeclarationNode(name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode) extends TopLevelExprNode
  case class AssignmentNode(address: AddressNode, value: ExprNode) extends TopLevelExprNode

  /**
    * An address of the form `a.b.c` and so on. The first element in the list is the leftmost name.
    */
  case class AddressNode(names: List[String]) extends Node {
    assert(names.nonEmpty)

    /**
      * Converts the address to an expression node, either a variable or a property access node.
      */
    def toExpression: ExprNode = {
      if (names.tail.nonEmpty) {
        ExprNode.PropertyAccessNode(ExprNode.VariableNode(names.head), names.tail)
      } else {
        ExprNode.VariableNode(names.head)
      }
    }
  }

  /**
    * Yield is a part of top-level expressions, because we don't want a programmer to yield in the middle of
    * an expression.
    */
  case class YieldNode(expr: ExprNode) extends TopLevelExprNode

  /**
    * The continuation of the construction is deferred to some other constructor or the internal construction
    * mechanism. Even though a continuation is only legal as the very last statement of a constructor block,
    * we parse it as a top-level expression to avoid ambiguities with function calls.
    */
  sealed trait ContinuationNode extends TopLevelExprNode
  case class ConstructorCallNode(name: Option[String], arguments: List[ExprNode]) extends ContinuationNode
  case class ConstructNode(arguments: List[ExprNode], withSuper: Option[ConstructorCallNode]) extends ContinuationNode
}

/**
  * Expressions returning some kind of value. Almost every statement in Lore is an expression.
  */
sealed trait ExprNode extends TopLevelExprNode
object ExprNode {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variable expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class VariableNode(name: String) extends ExprNode

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
  case class ConjunctionNode(expressions: List[ExprNode]) extends ExprNode
  case class DisjunctionNode(expressions: List[ExprNode]) extends ExprNode
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
  case class ConcatenationNode(expressions: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: List[ExprNode]) extends ExprNode

  /**
    * The unit tuple.
    */
  case object UnitNode extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode]) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class PropertyAccessNode(instance: ExprNode, names: List[String]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(statements: List[StmtNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or a constructor call. The parser can't decide between them based
    * on syntax, so the compiler will have to decide in later stages.
    */
  case class CallNode(name: String, qualifier: Option[String], arguments: List[ExprNode]) extends ExprNode

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(name: String, types: List[TypeExprNode], arguments: List[ExprNode]) extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and repetition expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode) extends ExprNode

  /**
    * @param deferCheck Whether the condition should be checked after the loop body.
    */
  case class RepeatWhileNode(condition: ExprNode, body: StmtNode, deferCheck: Boolean) extends ExprNode

  case class IterationNode(extractors: List[ExtractorNode], body: StmtNode) extends ExprNode
  case class ExtractorNode(variableName: String, collection: ExprNode)
}
