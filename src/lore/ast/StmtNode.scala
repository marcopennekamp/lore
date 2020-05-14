package lore.ast

import lore.ast.StmtNode._
import lore.compiler.phases.verification.{LocalVariable, VirtualMember}
import lore.definitions.CallTarget
import lore.types.{ProductType, Type}

/**
  * All statements and expressions.
  */
sealed trait StmtNode extends Node {
  private var _inferredType: Option[Type] = None

  def setInferredType(tpe: Type): Unit = {
    if (_inferredType.exists(_ != tpe)) {
      throw new RuntimeException(s"An inferred type for the node $this has already been set. Now a DIFFERENT type has been inferred. This is a compiler bug!")
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
  // TODO: If we switch to Dotty, we can use traits here (for the child properties). Our only real
  //       dependencies are fastparse and the graph library, so that may be possible quite soon.

  // Node types that are used by StmtVisitor.
  abstract class LeafNode extends StmtNode
  abstract class UnaryNode(val child: StmtNode) extends StmtNode
  abstract class BinaryNode(val child1: StmtNode, val child2: StmtNode) extends StmtNode
  abstract class TernaryNode(val child1: StmtNode, val child2: StmtNode, val child3: StmtNode) extends StmtNode
  abstract class XaryNode(val children: List[StmtNode]) extends StmtNode

  case class ReturnNode(expr: ExprNode) extends UnaryNode(expr)
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

sealed trait CallNode extends TopLevelExprNode {
  private var _target: CallTarget = _

  /**
    * The call target that this call node calls, which is resolved during function verification.
    */
  def target: CallTarget = _target
  def target_=(target: CallTarget): Unit = {
    assert(_target == null)
    _target = target
  }
}

object TopLevelExprNode {
  case class VariableDeclarationNode(
    name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode
  ) extends UnaryNode(value) with TopLevelExprNode
  case class AssignmentNode(address: ExprNode.AddressNode, value: ExprNode) extends BinaryNode(address, value) with TopLevelExprNode

  /**
    * Yield is a part of top-level expressions, because we don't want a programmer to yield in the middle of
    * an expression.
    */
  case class YieldNode(expr: ExprNode) extends UnaryNode(expr) with TopLevelExprNode

  /**
    * The continuation of the construction is deferred to some other constructor or the internal construction
    * mechanism. Even though a continuation is only legal as the very last statement of a constructor block,
    * we parse it as a top-level expression to avoid ambiguities with function calls.
    */
  sealed trait ContinuationNode extends TopLevelExprNode
  // TODO: Maybe rename to ThisCallNode, as this node doesn't refer to instantiation but rather calling another
  //       constructor from a constructor.
  case class ConstructorCallNode(name: Option[String], arguments: List[ExprNode]) extends XaryNode(arguments) with ContinuationNode with CallNode
  case class ConstructNode(arguments: List[ExprNode], withSuper: Option[ConstructorCallNode]) extends XaryNode(arguments) with ContinuationNode
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
  case class VariableNode(name: String) extends LeafNode with ExprNode with AddressNode {
    private var _variable: Option[LocalVariable] = None

    def setVariable(variable: LocalVariable): Unit = {
      if (_variable.exists(_ != variable)) {
        throw new RuntimeException(s"Variable node $this was assigned two different variables: ${_variable.get}, $variable. This is a compiler bug!")
      }
      _variable = Some(variable)
    }

    /**
      * The variable that this node refers to, which is resolved during function verification.
      */
    def variable: LocalVariable = {
      _variable.getOrElse(
        throw new RuntimeException(s"The variable for the node $this should have been set by now. This is a compiler bug!")
      )
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double) extends LeafNode with ExprNode
  case class IntLiteralNode(value: Int) extends LeafNode with ExprNode
  case class AdditionNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class NegationNode(expr: ExprNode) extends UnaryNode(expr) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean) extends LeafNode with ExprNode
  case class ConjunctionNode(expressions: List[ExprNode]) extends XaryNode(expressions) with ExprNode
  case class DisjunctionNode(expressions: List[ExprNode]) extends XaryNode(expressions) with ExprNode
  case class LogicalNotNode(expr: ExprNode) extends UnaryNode(expr) with ExprNode
  case class EqualsNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class LessThanNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanEqualsNode(left: ExprNode, right: ExprNode) extends BinaryNode(left, right) with ExprNode


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String) extends LeafNode with ExprNode
  case class ConcatenationNode(expressions: List[ExprNode]) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: List[ExprNode]) extends XaryNode(expressions) with ExprNode

  /**
    * The unit tuple.
    */
  case object UnitNode extends LeafNode with ExprNode {
    override def inferredType: Type = ProductType.UnitType
    override def setInferredType(tpe: Type): Unit = {
      // Do nothing. We don't want setInferredType to throw any errors when the inferred type is inevitably set
      // multiple times on this singleton object. We use this opportunity to catch any strange errors, however,
      // by asserting that the given type must be the unit type.
      assert(tpe == inferredType)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: List[ExprNode]) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode]) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: Rename to MemberAccessNode.
  case class PropertyAccessNode(instance: ExprNode, name: String) extends UnaryNode(instance) with ExprNode with AddressNode {
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
  case class BlockNode(statements: List[StmtNode]) extends XaryNode(statements) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or an instantiation call. The parser can't decide between them
    * based on syntax, so the compiler will have to decide in later stages.
    */
  case class SimpleCallNode(name: String, qualifier: Option[String], arguments: List[ExprNode]) extends XaryNode(arguments) with ExprNode with CallNode

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(name: String, types: List[TypeExprNode], arguments: List[ExprNode]) extends XaryNode(arguments) with ExprNode with CallNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and repetition expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode) extends TernaryNode(condition, onTrue, onFalse) with ExprNode

  /**
    * @param deferCheck Whether the condition should be checked after the loop body.
    */
  case class RepeatWhileNode(condition: ExprNode, body: StmtNode, deferCheck: Boolean) extends BinaryNode(condition, body) with ExprNode

  case class IterationNode(extractors: List[ExtractorNode], body: StmtNode) extends ExprNode
  case class ExtractorNode(variableName: String, collection: ExprNode) extends Node
}
