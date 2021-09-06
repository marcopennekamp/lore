package lore.compiler.syntax

import lore.compiler.core.Position
import lore.compiler.syntax.Node.{NameNode, NamedNode}
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

  def greaterThan(left: ExprNode, right: ExprNode, position: Position): LessThanNode = LessThanNode(right, left, position)
  def greaterThanEquals(left: ExprNode, right: ExprNode, position: Position): LessThanEqualsNode = LessThanEqualsNode(right, left, position)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String, position: Position) extends LeafNode with ExprNode
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
    nameNode: NameNode,
    argumentTypes: Vector[TypeExprNode],
    position: Position,
  ) extends LeafNode with ExprNode with NamedNode

  /**
    * A constructor instantiation with manual assignment of type arguments. A constructor call with inferred type
    * arguments is instead represented by [[SimpleCallNode]].
    */
  case class ConstructorNode(
    nameNode: NameNode,
    typeArguments: Vector[TypeExprNode],
    position: Position,
  ) extends LeafNode with ExprNode with NamedNode

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
    nameNode: NameNode,
    position: Position,
  ) extends UnaryNode(instance) with ExprNode with AddressNode with NamedNode

  case class ObjectMapNode(
    nameNode: NameNode,
    typeArguments: Option[Vector[TypeExprNode]],
    entries: Vector[ObjectEntryNode],
    position: Position,
  ) extends XaryNode(entries.map(_.expression)) with ExprNode with NamedNode

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
  // Symbol expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class SymbolValueNode(name: String, position: Position) extends LeafNode with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(expressions: Vector[TopLevelExprNode], position: Position) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node represents calling any sort of value, which can be an anonymous function, a multi-function typed as a
    * function, or a struct constructor. Full multi-function calls are always represented via [[SimpleCall]].
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
    nameNode: NameNode,
    arguments: Vector[ExprNode],
    position: Position,
  ) extends XaryNode(arguments) with ExprNode with NamedNode

  /**
    * The name of the dynamic function must be the first argument, as a string.
    */
  case class DynamicCallNode(
    nameLiteral: StringLiteralNode,
    resultType: TypeExprNode,
    arguments: Vector[ExprNode],
    position: Position,
  ) extends XaryNode(arguments) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class IfElseNode(
    condition: ExprNode, onTrue: TopLevelExprNode, onFalse: Option[TopLevelExprNode], position: Position,
  ) extends TernaryNode(condition, onTrue, onFalse.getOrElse(ExprNode.TupleNode(Vector.empty, position))) with ExprNode

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
}
