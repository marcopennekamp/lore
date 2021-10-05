package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.syntax.ExprNode
import lore.compiler.types.BasicType

object ExpressionFeedback {
  case class IllegalSymbolComparison(override val position: Position) extends Feedback.Error(position) {
    override def message = "Symbols are unordered and may not be compared using 'less than' or 'greater than'."
  }

  case class ImmutableAssignment(access: Expression.Access) extends Feedback.Error(access) {
    override def message = s"The variable or member $access may not be mutated."
  }

  case class IllegalModuleValue(module: GlobalModule, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The binding ${module.name} is a module. Modules cannot be used directly as" +
      s" expressions."
  }

  case class UnsafeInteger(node: ExprNode.IntLiteralNode) extends Feedback.Error(node) {
    override def message: String = s"The integer literal ${node.value} is outside the safe run-time range of" +
      s" ${BasicType.Int.minSafeInteger} and ${BasicType.Int.maxSafeInteger}. The Javascript runtime will not be able" +
      s" to properly store and process integers this large."
  }

  object FixedFunction {
    case class MultiFunctionExpected(name: NamePath, override val position: Position) extends Feedback.Error(position) {
      override def message = s"The binding $name must be a multi-function to be fixed."
    }
  }
}
