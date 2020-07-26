package lore.compiler.phases.verification

import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.transformer.StmtTransformer
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.core.Compilation.C
import lore.compiler.semantics.Registry
import lore.compiler.types.BasicType

// TODO: Write tests for this...
// TODO: What about string comparisons with localeCompare?

/**
  * Transforms comparison operations (==, =/=, <, <=, >, >=) of non-basic types into function calls, invoking
  * the standard functions areEqual, isLessThan, and isLessThanOrEqual.
  */
class ComparisonTransformer(implicit registry: Registry) extends StmtTransformer {
  override def transform(node: ExprNode.EqualsNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("areEqual", node, left, right)
  }

  override def transform(node: ExprNode.NotEqualsNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("areEqual", node, left, right).map { callNode =>
      // TODO: We need a more elegant solution. For example: States as case classes, nodes with fixed inferred types
      //       already set, making position copying easier, etc.
      val notNode = ExprNode.LogicalNotNode(callNode)
      notNode.state.position = node.position
      notNode.state.setInferredType(BasicType.Boolean)
      notNode
    }
  }

  override def transform(node: ExprNode.LessThanNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("isLessThan", node, left, right)
  }

  override def transform(node: ExprNode.LessThanEqualsNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("isLessThanOrEqual", node, left, right)
  }

  override def transform(node: ExprNode.GreaterThanNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("isLessThan", node, right, left)
  }

  override def transform(node: ExprNode.GreaterThanEqualsNode)(left: ExprNode, right: ExprNode): C[ExprNode] = {
    createCallIgnoringBasicTypes("isLessThanOrEqual", node, right, left)
  }

  /**
    * Creates a simple call node with the given function name and 'left' and 'right' as arguments IF either left's
    * or right's inferred type are NOT a basic type.
    */
  private def createCallIgnoringBasicTypes(functionName: String, comparisonNode: ExprNode, left: ExprNode, right: ExprNode): C[ExprNode] = {
    if (comparisonNode.state.inferredType != BasicType.Boolean) {
      throw CompilationException("The comparison's inferred type must be Boolean!")
    }

    (left.state.inferredType, right.state.inferredType) match {
      case (_: BasicType, _: BasicType) => Compilation.succeed(comparisonNode)
      case _ =>
        val callNode = ExprNode.SimpleCallNode(functionName, None, List(left, right))
        callNode.state.position = comparisonNode.position
        StatementVerification.verifySimpleFunctionCall(callNode).map { _ =>
          if (callNode.state.inferredType != BasicType.Boolean) {
            throw CompilationException(s"The function $functionName must return a Boolean value.")
          }
          assert(callNode.state.target != null)
          callNode
        }
    }
  }
}
