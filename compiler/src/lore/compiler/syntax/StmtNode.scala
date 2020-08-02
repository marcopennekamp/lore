package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.semantics.functions.{CallTarget, DynamicCallTarget, InternalCallTarget}
import lore.compiler.syntax.StmtNode._
import lore.compiler.syntax.TopLevelExprNode.CallNode

/**
  * All statements and expressions.
  */
sealed trait StmtNode extends Node
object StmtNode {
  // Node types that are used by StmtVisitor.
  sealed abstract class LeafNode extends ExprNode
  sealed abstract class UnaryNode(val child: StmtNode) extends StmtNode
  sealed abstract class BinaryNode(val child1: StmtNode, val child2: StmtNode) extends TopLevelExprNode
  sealed abstract class TernaryNode(val child1: StmtNode, val child2: StmtNode, val child3: StmtNode) extends ExprNode
  sealed abstract class XaryNode(val children: List[StmtNode]) extends TopLevelExprNode

  // TODO: Do we even need to differentiate between TopLevelExpressions and Statements or can we just give the return
  //       node an inferred type of nothing?
  case class ReturnNode(expr: ExprNode, position: Position) extends UnaryNode(expr)
}

/**
  * Expressions that are only allowed at the top-level, i.e. as immediate children of blocks or as a body
  * of conditionals and repetitions.
  *
  * This concept is used to implement the restriction that assignments can only stand in specific places.
  */
sealed trait TopLevelExprNode extends StmtNode
object TopLevelExprNode {
  sealed trait CallNode[T <: CallTarget] extends TopLevelExprNode

  case class VariableDeclarationNode(
    name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode, position: Position,
  ) extends UnaryNode(value) with TopLevelExprNode

  case class AssignmentNode(
    address: ExprNode.AddressNode, value: ExprNode, position: Position,
  ) extends BinaryNode(address, value) with TopLevelExprNode

  /**
    * The continuation of the construction is deferred to some other constructor or the internal construction
    * mechanism. Even though a continuation is only legal as the very last statement of a constructor block,
    * we parse it as a top-level expression to avoid ambiguities with function calls.
    */
  sealed trait ContinuationNode extends TopLevelExprNode

  case class ConstructorCallNode(
    name: Option[String], isSuper: Boolean, arguments: List[ExprNode], position: Position,
  ) extends XaryNode(arguments) with ContinuationNode with CallNode[InternalCallTarget]

  case class ConstructNode(
    arguments: List[ExprNode], withSuper: Option[ConstructorCallNode], position: Position,
  ) extends TopLevelExprNode with ContinuationNode
}

/**
  * Expressions returning some kind of value. Almost every statement in Lore is an expression.
  */
sealed trait ExprNode extends TopLevelExprNode
object ExprNode {
  // TODO: We could save a lot of nodes by turning Binary and Xary operations into their own node type (BinaryOperation,
  //       XaryOperation) and giving them an Operator type instead of creating a bajillion case classes.

  /**
    * A cross-cutting node trait signifying the possible target of an assignment.
    */
  sealed trait AddressNode extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variable expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class VariableNode(name: String, position: Position) extends LeafNode with ExprNode with AddressNode
  // TODO: If we introduce more complex referencing structures, such as global variables or global singleton objects,
  //       we cannot simply assume that a "variable node" is actually a local variable. In that case we will have to
  //       widen the notion of "having a local variable". For now, this seems to suffice, though. :)
  //       (Note: In the AST, it would only need a name change. We can later branch this one node into several different
  //        kinds of expressions depending on the kind of symbol that the name is referring to.)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double, position: Position) extends LeafNode with ExprNode
  case class IntLiteralNode(value: Int, position: Position) extends LeafNode with ExprNode
  case class AdditionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class NegationNode(expr: ExprNode, position: Position) extends UnaryNode(expr) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean, position: Position) extends LeafNode with ExprNode
  case class ConjunctionNode(expressions: List[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class DisjunctionNode(expressions: List[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class LogicalNotNode(expr: ExprNode, position: Position) extends UnaryNode(expr) with ExprNode
  case class EqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class LessThanNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanEqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  // TODO: We could save ourselves the greater than nodes.

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String, position: Position) extends LeafNode with ExprNode
  case class ConcatenationNode(expressions: List[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: List[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  /**
    * The unit tuple.
    */
  case class UnitNode(position: Position) extends LeafNode with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: List[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode], position: Position) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode, position: Position) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: Rename to MemberAccessNode.
  case class PropertyAccessNode(
    instance: ExprNode, name: String, position: Position,
  ) extends UnaryNode(instance) with ExprNode with AddressNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(statements: List[StmtNode], position: Position) extends XaryNode(statements) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or an instantiation call. The parser can't decide between them
    * based on syntax, so the compiler will have to decide in later stages.
    */
  case class SimpleCallNode(
    name: String, qualifier: Option[String], arguments: List[ExprNode], position: Position,
  ) extends XaryNode(arguments) with ExprNode with CallNode[InternalCallTarget]

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(
    name: String, types: List[TypeExprNode], arguments: List[ExprNode], position: Position
  ) extends XaryNode(arguments) with ExprNode with CallNode[InternalCallTarget]

  /**
    * Just like fixed function calls, dynamic calls have a different set of attributes than simple calls.
    *
    * The name of the dynamic function must be the first argument, as a string.
    */
  case class DynamicCallNode(
    resultType: TypeExprNode, arguments: List[ExprNode], position: Position
  ) extends XaryNode(arguments) with ExprNode with CallNode[DynamicCallTarget]

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(
    condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode, position: Position,
  ) extends TernaryNode(condition, onTrue, onFalse) with ExprNode

  /**
    * A cross-cutting node for loops.
    */
  sealed trait LoopNode extends ExprNode {
    def body: StmtNode
  }

  // TODO: Rename to WhileNode.
  case class RepetitionNode(
    condition: ExprNode, body: StmtNode, position: Position,
  ) extends BinaryNode(condition, body) with LoopNode

  // TODO: Rename to ForNode.
  case class IterationNode(
    extractors: List[ExtractorNode], body: StmtNode, position: Position,
  ) extends LoopNode

  case class ExtractorNode(
    variableName: String, collection: ExprNode, position: Position,
  ) extends Node
}
