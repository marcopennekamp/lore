package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.poem.PoemIntrinsic
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.syntax.ExprNode
import lore.compiler.types.BasicType

object ExpressionFeedback {
  case class ImmutableAssignment(access: Expression.Access) extends Feedback.Error(access) {
    override def message = s"The variable or member `$access` may not be mutated."
  }

  case class IllegalModuleValue(module: GlobalModule, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The binding ${module.name} is a module. Modules cannot be used directly as" +
      s" expressions."
  }

  case class UnsafeInteger(node: ExprNode.IntLiteralNode) extends Feedback.Error(node) {
    override def message: String = s"The integer literal ${node.value} is outside the safe run-time range of" +
      s" ${BasicType.Int.minimum} and ${BasicType.Int.maximum}. The runtime will not be able to properly " +
      s" store and process integers this large."
  }

  case class InvalidTotalCase(node: ExprNode.CondNode) extends Feedback.Error(node) {
    override def message: String = s"Cond expressions may only have a single total case (`true => ...`) as the last" +
      s" case. Mixing in total cases is not allowed because any subsequent cases are dead code."
  }

  object Intrinsic {
    case class NotFound(node: ExprNode.IntrinsicCallNode, name: String) extends Feedback.Error(node) {
      override def message: String = s"The intrinsic `$name` does not exist."
    }

    case class IllegalArity(node: ExprNode.IntrinsicCallNode, intrinsic: PoemIntrinsic, argumentCount: Int) extends Feedback.Error(node) {
      override def message: String = s"The intrinsic `${intrinsic.name}` expects ${intrinsic.arity} arguments, but got" +
        s" $argumentCount arguments."
    }
  }

  object FixedFunction {
    case class MultiFunctionExpected(name: NamePath, override val position: Position) extends Feedback.Error(position) {
      override def message = s"The binding $name must be a multi-function to be fixed."
    }
  }
}
