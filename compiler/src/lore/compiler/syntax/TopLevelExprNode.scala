package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.syntax.Node.{NameNode, NamePathNode, NamedNode, PathNamedNode}
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

  case class ReturnNode(expr: ExprNode, position: Position) extends UnaryNode(expr)

  case class VariableDeclarationNode(
    nameNode: NameNode,
    isMutable: Boolean,
    tpe: Option[TypeExprNode],
    value: ExprNode,
    position: Position,
  ) extends UnaryNode(value) with TopLevelExprNode with NamedNode

  case class AssignmentNode(
    address: ExprNode.AddressNode,
    value: ExprNode,
    position: Position,
  ) extends BinaryNode(address, value) with TopLevelExprNode
}

sealed trait ExprNode extends TopLevelExprNode
object ExprNode {
  /**
    * A cross-cutting node trait signifying the possible target of an assignment.
    */
  sealed trait AddressNode extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variable expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class VariableNode(namePathNode: NamePathNode, position: Position) extends LeafNode with ExprNode with AddressNode with PathNamedNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Literals.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class IntLiteralNode(value: Long, position: Position) extends LeafNode with ExprNode
  case class RealLiteralNode(value: Double, position: Position) extends LeafNode with ExprNode
  case class BoolLiteralNode(value: Boolean, position: Position) extends LeafNode with ExprNode
  case class StringLiteralNode(value: String, position: Position) extends LeafNode with ExprNode {
    def toNameNode: NameNode = NameNode(value, position)
  }
  case class SymbolLiteralNode(name: String, position: Position) extends LeafNode with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric operations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class AdditionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class NegationNode(expr: ExprNode, position: Position) extends UnaryNode(expr) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean operations and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ConjunctionNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class DisjunctionNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class LogicalNotNode(expr: ExprNode, position: Position) extends UnaryNode(expr) with ExprNode
  case class EqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class LessThanNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode, position: Position) extends BinaryNode(left, right) with ExprNode

  def greaterThan(left: ExprNode, right: ExprNode, position: Position): LessThanNode = LessThanNode(right, left, position)
  def greaterThanEquals(left: ExprNode, right: ExprNode, position: Position): LessThanEqualsNode = LessThanEqualsNode(right, left, position)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String operations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ConcatenationNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Anonymous functions and function values.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class AnonymousFunctionNode(
    parameters: Vector[AnonymousFunctionParameterNode],
    body: ExprNode,
    position: Position,
  ) extends ExprNode

  case class AnonymousFunctionParameterNode(
    nameNode: NameNode,
    tpe: Option[TypeExprNode],
    position: Position,
  ) extends NamedNode

  case class FixedFunctionNode(
    namePathNode: NamePathNode,
    argumentTypes: Vector[TypeExprNode],
    position: Position,
  ) extends LeafNode with ExprNode with PathNamedNode

  /**
    * A constructor instantiation with manual assignment of type arguments. A constructor call with inferred type
    * arguments is instead represented by [[SimpleCallNode]].
    */
  case class ConstructorNode(
    namePathNode: NamePathNode,
    typeArguments: Vector[TypeExprNode],
    position: Position,
  ) extends LeafNode with ExprNode with PathNamedNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Collection expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: Vector[ExprNode], position: Position) extends XaryNode(expressions) with ExprNode
  case class MapNode(entries: Vector[KeyValueNode], position: Position) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode, position: Position) extends Node
  case class AppendNode(collection: ExprNode, element: ExprNode, position: Position) extends BinaryNode(collection, element) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Even though member access might be parsed as name paths, there is still a need for MemberAccessNodes to represent
    * expressions such as `(if a then b else c).member`.
    */
  case class MemberAccessNode(
    instance: ExprNode,
    nameNode: NameNode,
    position: Position,
  ) extends UnaryNode(instance) with ExprNode with AddressNode with NamedNode

  case class ObjectMapNode(
    namePathNode: NamePathNode,
    typeArguments: Option[Vector[TypeExprNode]],
    entries: Vector[ObjectEntryNode],
    position: Position,
  ) extends XaryNode(entries.map(_.expression)) with ExprNode with PathNamedNode

  case class ObjectEntryNode(nameNode: NameNode, expression: ExprNode, position: Position) extends NamedNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shape expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ShapeValueNode(
    properties: Vector[ShapeValuePropertyNode],
    position: Position
  ) extends XaryNode(properties.map(_.expression)) with ExprNode

  case class ShapeValuePropertyNode(nameNode: NameNode, expression: ExprNode, position: Position) extends NamedNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(expressions: Vector[TopLevelExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node represents calling any sort of value, which can be an anonymous function, a multi-function typed as a
    * function, or a struct constructor. Full multi-function calls are always represented via [[SimpleCallNode]].
    */
  case class CallNode(
    target: ExprNode,
    arguments: Vector[ExprNode],
    position: Position,
  ) extends ExprNode

  /**
    * A simple call represents calling a named entity, which will usually be a multi-function.
    *
    * Simple calls are split from more complex target calls because they are the only way to call a multi-function
    * directly. In all other cases, if the multi-function is used as a variable, it is immediately coerced to a
    * function type. Splitting these concerns at the syntax level leads to a simpler implementation down the pipeline.
    */
  case class SimpleCallNode(
    namePathNode: NamePathNode,
    arguments: Vector[ExprNode],
    position: Position,
  ) extends XaryNode(arguments) with ExprNode with PathNamedNode

  /**
    * The name of the intrinsic function must be the first argument, as a string.
    */
  case class IntrinsicCallNode(
    nameLiteral: StringLiteralNode,
    resultType: TypeExprNode,
    arguments: Vector[ExprNode],
    position: Position,
  ) extends XaryNode(arguments) with ExprNode

  /**
    * Constructs a CallNode or a SimpleCallNode from a pipe operator.
    */
  def pipe(argument: ExprNode, target: ExprNode, position: Position): ExprNode = {
    target match {
      case VariableNode(name, position) => SimpleCallNode(name, Vector(argument), position)
      case CallNode(target, arguments, position) => CallNode(target, argument +: arguments, position)
      case SimpleCallNode(nameNode, arguments, position) => SimpleCallNode(nameNode, argument +: arguments, position)
      case IntrinsicCallNode(nameLiteral, resultType, arguments, position) => IntrinsicCallNode(nameLiteral, resultType, argument +: arguments, position)
      case expression => CallNode(expression, Vector(argument), expression.position)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class IfElseNode(
    condition: ExprNode,
    onTrue: TopLevelExprNode,
    onFalse: Option[TopLevelExprNode],
    position: Position,
  ) extends TernaryNode(condition, onTrue, onFalse.getOrElse(ExprNode.TupleNode(Vector.empty, position))) with ExprNode

  case class CondNode(
    cases: Vector[CondCaseNode],
    position: Position,
  ) extends ExprNode

  case class CondCaseNode(
    condition: ExprNode,
    body: TopLevelExprNode,
    position: Position,
  ) extends Node

  /**
    * A cross-cutting node for loops.
    */
  sealed trait LoopNode extends ExprNode {
    def body: TopLevelExprNode
  }

  case class WhileNode(
    condition: ExprNode,
    body: TopLevelExprNode,
    position: Position,
  ) extends BinaryNode(condition, body) with LoopNode

  case class ForNode(
    extractors: Vector[ExtractorNode],
    body: TopLevelExprNode,
    position: Position,
  ) extends LoopNode

  case class ExtractorNode(
    nameNode: NameNode,
    collection: ExprNode,
    position: Position,
  ) extends NamedNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type ascriptions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class AscriptionNode(value: ExprNode, expectedType: TypeExprNode, position: Position) extends UnaryNode(value) with ExprNode
}
