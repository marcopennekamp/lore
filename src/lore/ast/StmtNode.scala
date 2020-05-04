package lore.ast

import lore.ast.StmtNode._
import lore.compiler.phases.verification.{LocalVariable, VirtualMember}
import lore.types.Type

/**
  * All statements and expressions.
  */
sealed trait StmtNode extends Node {
  private var _inferredType: Option[Type] = None

  def setInferredType(tpe: Type): Unit = {
    if (_inferredType.isDefined) {
      throw new RuntimeException(s"The inferred type for the node $this has already been set. This is a compiler bug!")
    }
    _inferredType = Some(tpe)
  }

  def inferredType: Type = {
    _inferredType.getOrElse(
      throw new RuntimeException(s"The inferred type for the node $this should have been set by now. This is a compiler bug!")
    )
  }
}

object StmtNode {
  case class ReturnNode(expr: ExprNode) extends StmtNode with UnaryNode

  // Node types that are used by StmtVisitor.
  sealed trait LeafNode extends StmtNode
  sealed trait UnaryNode extends StmtNode
  sealed trait BinaryNode extends StmtNode
  sealed trait TernaryNode extends StmtNode
  sealed trait XaryNode extends StmtNode
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
  case class VariableDeclarationNode(name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode) extends TopLevelExprNode with UnaryNode
  case class AssignmentNode(address: ExprNode.AddressNode, value: ExprNode) extends TopLevelExprNode with UnaryNode

  /**
    * Yield is a part of top-level expressions, because we don't want a programmer to yield in the middle of
    * an expression.
    */
  case class YieldNode(expr: ExprNode) extends TopLevelExprNode with UnaryNode

  /**
    * The continuation of the construction is deferred to some other constructor or the internal construction
    * mechanism. Even though a continuation is only legal as the very last statement of a constructor block,
    * we parse it as a top-level expression to avoid ambiguities with function calls.
    */
  sealed trait ContinuationNode extends TopLevelExprNode
  case class ConstructorCallNode(name: Option[String], arguments: List[ExprNode]) extends ContinuationNode with XaryNode
  case class ConstructNode(arguments: List[ExprNode], withSuper: Option[ConstructorCallNode]) extends ContinuationNode with XaryNode
}

/**
  * Expressions returning some kind of value. Almost every statement in Lore is an expression.
  */
sealed trait ExprNode extends TopLevelExprNode
object ExprNode {
  /**
    * A cross-cutting node trait signifying the possible target of an assignment.
    */
  sealed trait AddressNode extends ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variable expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class VariableNode(name: String) extends ExprNode with LeafNode with AddressNode {
    private var _variable: LocalVariable = _

    /**
      * The variable that this node refers to, which is resolved during function verification.
      */
    def variable: LocalVariable = _variable
    def variable_=(variable: LocalVariable): Unit = {
      assert(_variable == null)
      _variable = variable
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double) extends ExprNode with LeafNode
  case class IntLiteralNode(value: Int) extends ExprNode with LeafNode
  case class AdditionNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class SubtractionNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class DivisionNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class NegationNode(expr: ExprNode) extends ExprNode with UnaryNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean) extends ExprNode with LeafNode
  case class ConjunctionNode(expressions: List[ExprNode]) extends ExprNode with XaryNode
  case class DisjunctionNode(expressions: List[ExprNode]) extends ExprNode with XaryNode
  case class LogicalNotNode(expr: ExprNode) extends ExprNode with UnaryNode
  case class EqualsNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class LessThanNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class GreaterThanNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode
  case class GreaterThanEqualsNode(left: ExprNode, right: ExprNode) extends ExprNode with BinaryNode


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String) extends ExprNode with LeafNode
  case class ConcatenationNode(expressions: List[ExprNode]) extends ExprNode with XaryNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: List[ExprNode]) extends ExprNode with XaryNode

  /**
    * The unit tuple.
    */
  case object UnitNode extends ExprNode with LeafNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: List[ExprNode]) extends ExprNode with XaryNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode]) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: Rename to MemberAccessNode.
  case class PropertyAccessNode(instance: ExprNode, name: String) extends ExprNode with UnaryNode with AddressNode {
    private var _member: VirtualMember = _

    /**
      * The virtual member that this property node accesses, which is resolved during function verification.
      */
    def member: VirtualMember = _member
    def member_=(member: VirtualMember): Unit = {
      assert(_member == null)
      _member = member
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(statements: List[StmtNode]) extends ExprNode with XaryNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or a constructor call. The parser can't decide between them based
    * on syntax, so the compiler will have to decide in later stages.
    */
  case class CallNode(name: String, qualifier: Option[String], arguments: List[ExprNode]) extends ExprNode with XaryNode

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(name: String, types: List[TypeExprNode], arguments: List[ExprNode]) extends ExprNode with XaryNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and repetition expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode) extends ExprNode with TernaryNode

  /**
    * @param deferCheck Whether the condition should be checked after the loop body.
    */
  case class RepeatWhileNode(condition: ExprNode, body: StmtNode, deferCheck: Boolean) extends ExprNode with BinaryNode

  case class IterationNode(extractors: List[ExtractorNode], body: StmtNode) extends ExprNode
  case class ExtractorNode(variableName: String, collection: ExprNode)
}
