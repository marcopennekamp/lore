package lore.compiler.ast

import lore.compiler.ast.StmtNode._
import lore.compiler.core.CompilationException
import lore.compiler.functions.{CallTarget, DynamicCallTarget, InternalCallTarget}
import lore.compiler.phases.verification.{LocalVariable, VirtualMember}
import lore.compiler.types.{ProductType, Type}

/**
  * All statements and expressions.
  */
sealed trait StmtNode extends Node {
  override def state: StmtNode.State
}

object StmtNode {
  // TODO: If we switch to Dotty, we can use traits here (for the child properties). Our only real
  //       dependencies are fastparse and the graph library, so that may be possible quite soon.

  trait State extends Node.State {
    /**
      * The result type of the statement. If None, the type has not been inferred yet. This property is meant to
      * cache the result of type inference and is set by the type inference mechanism.
      */
    private var _inferredType: Option[Type] = None

    // TODO: Change to _= function.
    def setInferredType(tpe: Type): Unit = {
      if (_inferredType.exists(_ != tpe)) {
        throw CompilationException(s"An inferred type for the node $this has already been set. Now a DIFFERENT type has been inferred.")
      }
      _inferredType = Some(tpe)
    }

    def inferredType: Type = {
      _inferredType.getOrElse(
        throw CompilationException(s"The inferred type for the node $this should have been set by now.")
      )
    }
  }

  class DefaultState extends State
  class DefaultLocalVariableState extends LocalVariableState
  class DefaultMemberState extends MemberState

  trait LocalVariableState extends State {
    private var _variable: Option[LocalVariable] = None

    // TODO: Change to _= function.
    def setVariable(variable: LocalVariable, node: StmtNode): Unit = {
      if (_variable.exists(_ != variable)) {
        throw CompilationException(s"Variable node $this was assigned two different variables: ${_variable.get}, $variable.")
      }
      _variable = Some(variable)
    }

    /**
      * The variable that this node refers to, which is resolved during function verification.
      */
    def variable: LocalVariable = {
      _variable.getOrElse(
        throw CompilationException(s"The variable for the node $this should have been set by now.")
      )
    }
  }

  trait MemberState extends State {
    private var _member: VirtualMember = _

    /**
      * The virtual member that this property node accesses, which is resolved during function verification.
      */
    def member: VirtualMember = _member
    def member_=(member: VirtualMember): Unit = {
      if (_member != null && _member != member) {
        throw CompilationException(s"Member already assigned. Old: ${_member}. New: $member.")
      }
      _member = member
    }
  }

  // Node types that are used by StmtVisitor.
  sealed abstract class LeafNode extends ExprNode
  sealed abstract class UnaryNode(val child: StmtNode) extends StmtNode
  sealed abstract class BinaryNode(val child1: StmtNode, val child2: StmtNode) extends TopLevelExprNode
  sealed abstract class TernaryNode(val child1: StmtNode, val child2: StmtNode, val child3: StmtNode) extends ExprNode
  sealed abstract class XaryNode(val children: List[StmtNode]) extends TopLevelExprNode

  // TODO: Do we even need to differentiate between TopLevelExpressions and Statements or can we just give the return
  //       node an inferred type of nothing?
  case class ReturnNode(expr: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends UnaryNode(expr)
}

/**
  * Expressions that are only allowed at the top-level, i.e. as immediate children of blocks or as a body
  * of conditionals and repetitions.
  *
  * This concept is used to implement the restriction that assignments can only stand in specific places.
  */
sealed trait TopLevelExprNode extends StmtNode

sealed trait CallNode[T <: CallTarget] extends TopLevelExprNode {
  override def state: CallNode.State[T]
}
object CallNode {
  trait State[T <: CallTarget] extends StmtNode.State {
    private var _target: T = _

    /**
      * The call target that this call node calls, which is resolved during function verification.
      */
    def target: T = _target
    def target_=(target: T): Unit = {
      println(_target)
      assert(_target == null)
      _target = target
    }
  }
  class DefaultState[T <: CallTarget] extends State[T]
}

object TopLevelExprNode {
  case class VariableDeclarationNode(
    name: String, isMutable: Boolean, tpe: Option[TypeExprNode], value: ExprNode,
    state: LocalVariableState = new DefaultLocalVariableState,
  ) extends UnaryNode(value) with TopLevelExprNode

  case class AssignmentNode(
    address: ExprNode.AddressNode, value: ExprNode, state: StmtNode.State = new StmtNode.DefaultState,
  ) extends BinaryNode(address, value) with TopLevelExprNode

  /**
    * The continuation of the construction is deferred to some other constructor or the internal construction
    * mechanism. Even though a continuation is only legal as the very last statement of a constructor block,
    * we parse it as a top-level expression to avoid ambiguities with function calls.
    */
  sealed trait ContinuationNode extends TopLevelExprNode

  case class ConstructorCallNode(
    name: Option[String], arguments: List[ExprNode],
    state: CallNode.State[InternalCallTarget] = new CallNode.DefaultState,
  ) extends XaryNode(arguments) with ContinuationNode with CallNode[InternalCallTarget]

  case class ConstructNode(
    arguments: List[ExprNode], withSuper: Option[ConstructorCallNode],
    state: StmtNode.State = new StmtNode.DefaultState,
  ) extends XaryNode(arguments) with ContinuationNode
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
  case class VariableNode(
    name: String,
    state: LocalVariableState = new DefaultLocalVariableState,
  ) extends LeafNode with ExprNode with AddressNode
  // TODO: If we introduce more complex referencing structures, such as global variables or global singleton objects,
  //       we cannot simply assume that a "variable node" is actually a local variable. In that case we will have to
  //       widen the notion of "having a local variable". For now, this seems to suffice, though. :)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Numeric expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class RealLiteralNode(value: Double, state: StmtNode.State = new StmtNode.DefaultState) extends LeafNode with ExprNode
  case class IntLiteralNode(value: Int, state: StmtNode.State = new StmtNode.DefaultState) extends LeafNode with ExprNode
  case class AdditionNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class SubtractionNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class MultiplicationNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class DivisionNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class NegationNode(expr: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends UnaryNode(expr) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Boolean expressions and comparison operators.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BoolLiteralNode(value: Boolean, state: StmtNode.State = new StmtNode.DefaultState) extends LeafNode with ExprNode
  case class ConjunctionNode(expressions: List[ExprNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(expressions) with ExprNode
  case class DisjunctionNode(expressions: List[ExprNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(expressions) with ExprNode
  case class LogicalNotNode(expr: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends UnaryNode(expr) with ExprNode
  case class EqualsNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class NotEqualsNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class LessThanNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class LessThanEqualsNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode
  case class GreaterThanEqualsNode(left: ExprNode, right: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends BinaryNode(left, right) with ExprNode


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // String expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class StringLiteralNode(value: String, state: StmtNode.State = new StmtNode.DefaultState) extends LeafNode with ExprNode
  case class ConcatenationNode(expressions: List[ExprNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class TupleNode(expressions: List[ExprNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(expressions) with ExprNode

  /**
    * The unit tuple.
    */
  case class UnitNode(state: StmtNode.State = new UnitNode.DefaultUnitState) extends LeafNode with ExprNode

  object UnitNode {
    class DefaultUnitState extends StmtNode.DefaultState {
      override def inferredType: Type = ProductType.UnitType
      override def setInferredType(tpe: Type): Unit = {
        // Do nothing. We don't want setInferredType to throw any errors when the inferred type is inevitably set
        // multiple times on this singleton object. We use this opportunity to catch any strange errors, however,
        // by asserting that the given type must be the unit type.
        assert(tpe == inferredType)
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // List expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class ListNode(expressions: List[ExprNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(expressions) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Map expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class MapNode(kvs: List[KeyValueNode], state: StmtNode.State = new StmtNode.DefaultState) extends ExprNode
  case class KeyValueNode(key: ExprNode, value: ExprNode, state: StmtNode.State = new StmtNode.DefaultState) extends Node

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Object expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TODO: Rename to MemberAccessNode.
  case class PropertyAccessNode(
    instance: ExprNode, name: String,
    state: MemberState = new DefaultMemberState
  ) extends UnaryNode(instance) with ExprNode with AddressNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Block expressions. Note that blocks can hold statements.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class BlockNode(statements: List[StmtNode], state: StmtNode.State = new StmtNode.DefaultState) extends XaryNode(statements) with ExprNode

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function calls.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * A call node can both be a multi-function call or an instantiation call. The parser can't decide between them
    * based on syntax, so the compiler will have to decide in later stages.
    */
  case class SimpleCallNode(
    name: String, qualifier: Option[String], arguments: List[ExprNode],
    state: CallNode.State[InternalCallTarget] = new CallNode.DefaultState,
  ) extends XaryNode(arguments) with ExprNode with CallNode[InternalCallTarget]

  /**
    * Since fixed function calls also require type arguments, they can be differentiated from call nodes.
    */
  case class FixedFunctionCallNode(
    name: String, types: List[TypeExprNode], arguments: List[ExprNode],
    state: CallNode.State[InternalCallTarget] = new CallNode.DefaultState
  ) extends XaryNode(arguments) with ExprNode with CallNode[InternalCallTarget]

  /**
    * Just like fixed function calls, dynamic calls have a different set of attributes than simple calls.
    *
    * The name of the dynamic function must be the first argument, as a string.
    */
  case class DynamicCallNode(
    resultType: TypeExprNode, arguments: List[ExprNode],
    state: CallNode.State[DynamicCallTarget] = new CallNode.DefaultState
  ) extends XaryNode(arguments) with ExprNode with CallNode[DynamicCallTarget]

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conditional and loop expressions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * @param onTrue Equals UnitNode if it doesn't exist.
    * @param onFalse Equals UnitNode if it doesn't exist.
    */
  case class IfElseNode(
    condition: ExprNode, onTrue: StmtNode, onFalse: StmtNode,
    state: StmtNode.State = new StmtNode.DefaultState,
  ) extends TernaryNode(condition, onTrue, onFalse) with ExprNode

  /**
    * A cross-cutting node for loops.
    */
  sealed trait LoopNode extends ExprNode {
    def body: StmtNode
  }

  case class RepetitionNode(
    condition: ExprNode, body: StmtNode,
    state: StmtNode.State = new StmtNode.DefaultState,
  ) extends BinaryNode(condition, body) with LoopNode

  case class IterationNode(
    extractors: List[ExtractorNode], body: StmtNode,
    state: StmtNode.State = new StmtNode.DefaultState,
  ) extends LoopNode

  case class ExtractorNode(
    variableName: String, collection: ExprNode,
    state: LocalVariableState = new DefaultLocalVariableState,
  ) extends Node
}
