package lore.compiler.phases.verification

import lore.compiler.ast.ExprNode.LoopNode
import lore.compiler.ast.{CallNode, ExprNode, StmtNode}
import lore.compiler.core.{Compilation, Registry}
import lore.compiler.core.Compilation.Verification
import lore.compiler.feedback.{Error, Position}
import lore.compiler.functions.{FunctionSignature, InternalCallTarget}
import lore.compiler.phases.verification.FunctionVerification.IllegallyTypedExpression
import lore.compiler.phases.verification.FunctionVerificationVisitor.{AmbiguousCall, EmptyFit}
import lore.compiler.types.{BasicType, ListType, ProductType, Type}

/**
  * Provides functions for verifying statement nodes. Mainly used by [[FunctionVerificationVisitor]], but also used
  * by transformers to verify & type nodes after FunctionVerification has already taken place.
  */
object StatementVerification {
  implicit class StmtNodeExtension(node: StmtNode) {
    /**
      * Assigns the given type to the node and returns the type.
      */
    def typed(tpe: Type): Verification = {
      node.state.setInferredType(tpe)
      Verification.succeed
    }
  }

  /**
    * Whether the given statement's inferred type is a subtype of one of the expected types.
    */
  def havingSubtype(statement: StmtNode, supertypes: Type*): Verification = {
    if (!supertypes.exists(expected => statement.state.inferredType <= expected)) {
      Compilation.fail(IllegallyTypedExpression(statement, supertypes.toList))
    } else Verification.succeed
  }

  def beingNumber(statement: StmtNode): Verification = {
    havingSubtype(statement, BasicType.Int, BasicType.Real)
  }

  def beingNumbers(statements: StmtNode*): Verification = {
    statements.toList.map(beingNumber).simultaneous.verification
  }

  def beingBoolean(statement: StmtNode): Verification = {
    havingSubtype(statement, BasicType.Boolean)
  }

  def beingBooleans(statements: StmtNode*): Verification = {
    statements.toList.map(beingBoolean).simultaneous.verification
  }

  def typeBinaryNumbers(node: StmtNode, left: StmtNode, right: StmtNode): Verification = {
    beingNumbers(left, right).flatMap { _ =>
      if (left.state.inferredType == BasicType.Real || right.state.inferredType == BasicType.Real) {
        node.typed(BasicType.Real)
      } else { // Both operands are integers.
        node.typed(BasicType.Int)
      }
    }
  }

  def typeXaryBooleans(node: StmtNode, nodes: List[StmtNode]): Verification = {
    beingBooleans(nodes: _*).flatMap { _ =>
      node.typed(BasicType.Boolean)
    }
  }

  case class WrongNumberOfArguments(signature: FunctionSignature, callPos: Position) extends Error(callPos) {
    override def message: String = s"The function/constructor ${signature.name} was called with the wrong number of arguments." +
      s" Expected: ${signature.parameters.size}."
  }

  /**
    * Checks that the given arguments adhere to the given signature.
    *
    * We are assuming that the signature is fixed, so don't use this for dispatched functions.
    */
  def adheringToSignature(arguments: List[ExprNode], signature: FunctionSignature, callSite: Position): Verification = {
    val parameterTypes = signature.parameters.map(_.tpe)
    if (parameterTypes.size != arguments.size) {
      Compilation.fail(WrongNumberOfArguments(signature, callSite))
    } else {
      parameterTypes.zip(arguments).map { case (parameterType, argument) =>
        havingSubtype(argument, parameterType)
      }.simultaneous.verification
    }
  }

  /**
    * Checks that the given call target can be called with the given arguments, then assigns the target to the node
    * and also types it. Do NOT use this for dispatched functions!
    */
  def typeCall(node: CallNode[InternalCallTarget], target: InternalCallTarget, arguments: List[ExprNode]): Verification = {
    adheringToSignature(arguments, target.signature, node.position).flatMap { _ =>
      assignTarget(node, target)
    }
  }

  /**
    * Assigns the target to the node and types it with the target's output type.
    */
  def assignTarget(node: CallNode[InternalCallTarget], target: InternalCallTarget): Verification = {
    node.state.target = target
    node.typed(target.signature.outputType)
  }

  def typeLoop(node: LoopNode): Verification = {
    val bodyType = node.body.state.inferredType
    val loopType = if (bodyType == ProductType.UnitType) {
      ProductType.UnitType
    } else {
      ListType(bodyType)
    }
    node.typed(loopType)
  }

  /**
    * Verifies a simple function call. Cannot be used to verify constructor calls!
    */
  def verifySimpleFunctionCall(node: ExprNode.SimpleCallNode)(implicit registry: Registry): Verification = {
    assert(node.qualifier.isEmpty)
    registry.resolveMultiFunction(node.name, node.position).flatMap { mf =>
      val inputType = ProductType(node.arguments.map(_.state.inferredType))
      implicit val callPosition: Position = node.position
      mf.min(inputType) match {
        case Nil => Compilation.fail(EmptyFit(mf))
        case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, min))
        case List(functionDefinition) =>
          functionDefinition.instantiate(inputType).flatMap { instance =>
            assignTarget(node, instance)
          }
      }
    }
  }
}
