package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.BinaryOperator
import lore.compiler.types.{BasicType, SymbolType}

object BuiltinsTransformation {

  sealed trait ComparisonFunction {
    def name: String
  }

  object ComparisonFunction {
    case object AreEqual extends ComparisonFunction {
      override val name = "areEqual"
    }

    case object IsLessThan extends ComparisonFunction {
      override val name = "isLessThan"
    }

    case object IsLessThanOrEqual extends ComparisonFunction {
      override val name = "isLessThanOrEqual"
    }
  }

  case class IllegalSymbolComparison(override val position: Position) extends Feedback.Error(position) {
    override def message = "Symbols are unordered and may not be compared using 'less than' or 'greater than'."
  }

  /**
    * Transforms comparison operations (==, !=, <, <=, >, >=) of non-basic types into function calls, invoking the
    * standard functions areEqual, isLessThan, and isLessThanOrEqual. If this comparison is comparing basic types,
    * it instead applies the basic operator for comparison.
    *
    * Results in a compilation error if areEqual, isLessThan, or isLessThanOrEqual doesn't return a boolean value.
    */
  def transformComparison(
    comparisonFunction: ComparisonFunction,
    basicOperator: BinaryOperator,
    left: Expression,
    right: Expression,
    position: Position,
  )(implicit registry: Registry): Compilation[Expression] = {
    (left.tpe, right.tpe) match {
      case (_: BasicType, _: BasicType) => Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position).compiled

      case (_: SymbolType, _: SymbolType) => comparisonFunction match {
        case ComparisonFunction.AreEqual =>
          // Because symbol values are interned, they can be compared by reference, using the equality operator.
          Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position).compiled

        case ComparisonFunction.IsLessThan | ComparisonFunction.IsLessThanOrEqual => Compilation.fail(IllegalSymbolComparison(position))
      }

      case _ =>
        CallTransformation
          .multiFunctionCall(comparisonFunction.name, Vector(left, right), position)
          .flatMap(CallVerification.ensureOutputType(BasicType.Boolean))
    }
  }

}
