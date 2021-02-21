package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.syntax.TopLevelExprNode._

/**
  * Top-level expression are all regular expressions and additionally expressions that are only allowed at the
  * top-level, i.e. as immediate children of blocks or as a body of conditionals and repetitions. This concept is used
  * to implement the restriction that assignments and returns can only stand in specific places.
  */
sealed trait TopLevelExprNode extends Node
object TopLevelExprNode {
  // Node types that are used by StmtVisitor.
  sealed abstract class LeafNode extends ExprNode
  sealed abstract class UnaryNode(val child: TopLevelExprNode) extends TopLevelExprNode
  sealed abstract class BinaryNode(val child1: TopLevelExprNode, val child2: TopLevelExprNode) extends TopLevelExprNode
  sealed abstract class TernaryNode(val child1: TopLevelExprNode, val child2: TopLevelExprNode, val child3: TopLevelExprNode) extends ExprNode
  sealed abstract class XaryNode(val children: Vector[TopLevelExprNode]) extends TopLevelExprNode

  sealed trait CallNode[T <: CallTarget] extends TopLevelExprNode

  case class ReturnNode(expr: ExprNode, position: Position) extends UnaryNode(expr)

  case class VariableDeclarationNode(
    name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode, position: Position,
  ) extends UnaryNode(value) with TopLevelExprNode

  case class AssignmentNode(
    address: ExprNode.AddressNode, value: ExprNode, position: Position,
  ) extends BinaryNode(address, value) with TopLevelExprNode
}

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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double, position: Position) extends LeafNode with ExprNode
  case class IntLiteralNode(value: Long, position: Position) extends LeafNode with ExprNode
  case class AdditionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class NegationNode(expr: ExprNode, position: Position) extends UnaryNode(expr) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean, position: Position) extends LeafNode with ExprNode
  case class ConjunctionNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class DisjunctionNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
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
  case class ConcatenationNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  /**
    * The unit tuple.
    */
  case class UnitNode(position: Position) extends LeafNode with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Collection expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class MapNode(kvs: Vector[KeyValueNode], position: Position) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode, position: Position) extends Node
  case class AppendNode(collection: ExprNode, element: ExprNode, position: Position) extends BinaryNode(collection, element) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MemberAccessNode(
    instance: ExprNode,
    name: String,
    position: Position,
  ) extends UnaryNode(instance) with ExprNode with AddressNode

  case class ObjectMapNode(
    structName: String,
    entries: Vector[ObjectEntryNode],
    position: Position,
  ) extends XaryNode(entries.map(_.expression)) with ExprNode
  case class ObjectEntryNode(name: String, expression: ExprNode, position: Position) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shape expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ShapeValueNode(
    properties: Vector[ShapeValuePropertyNode],
    position: Position
  ) extends XaryNode(properties.map(_.expression)) with ExprNode
  case class ShapeValuePropertyNode(name: String, expression: ExprNode, position: Position) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(expressions: Vector[TopLevelExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or an instantiation call. The parser can't decide between them
    * based on syntax, so the compiler will have to decide in later stages.
    */
  case class SimpleCallNode(
    name: String, arguments: Vector[ExprNode], position: Position,
  ) extends XaryNode(arguments) with ExprNode with CallNode[CallTarget.Internal]

  /**
    * An explicit call to a value that is not a variable, which would have been covered by simple call node.
    *
    * TODO: Once multi-functions become variables with their own types, this needs to be merged into SimpleCallNode
    *       and simply become CallNode.
    */
  case class ValueCallNode(
    value: ExprNode, arguments: Vector[ExprNode], position: Position
  ) extends XaryNode(arguments) with ExprNode with CallNode[CallTarget.Internal]

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(
    name: String, types: Vector[TypeExprNode], arguments: Vector[ExprNode], position: Position
  ) extends XaryNode(arguments) with ExprNode with CallNode[CallTarget.Internal]

  /**
    * Just like fixed function calls, dynamic calls have a different set of attributes than simple calls.
    *
    * The name of the dynamic function must be the first argument, as a string.
    */
  case class DynamicCallNode(
    resultType: TypeExprNode, arguments: Vector[ExprNode], position: Position
  ) extends XaryNode(arguments) with ExprNode with CallNode[CallTarget.Dynamic]

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(
    condition: ExprNode, onTrue: TopLevelExprNode, onFalse: TopLevelExprNode, position: Position,
  ) extends TernaryNode(condition, onTrue, onFalse) with ExprNode

  /**
    * A cross-cutting node for loops.
    */
  sealed trait LoopNode extends ExprNode {
    def body: TopLevelExprNode
  }

  case class WhileNode(
    condition: ExprNode, body: TopLevelExprNode, position: Position,
  ) extends BinaryNode(condition, body) with LoopNode

  case class ForNode(
    extractors: Vector[ExtractorNode], body: TopLevelExprNode, position: Position,
  ) extends LoopNode

  case class ExtractorNode(
    variableName: String, collection: ExprNode, position: Position,
  ) extends Node
}
