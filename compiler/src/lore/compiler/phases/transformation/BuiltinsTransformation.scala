package lore.compiler.phases.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
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
    * Transforms comparison operations (==, !=, <, <=, >, >=) into the proper expressions, invoking the standard
    * functions areEqual, isLessThan, and isLessThanOrEqual for complex comparisons. Results in an error if areEqual,
    * isLessThan, or isLessThanOrEqual doesn't return a boolean value.
    *
    * If an expression cannot be created, the function falls back to [[Expression.Hole]] with a Boolean type.
    */
  def transformComparison(
    comparisonFunction: ComparisonFunction,
    basicOperator: BinaryOperator,
    left: Expression,
    right: Expression,
    position: Position,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    (left.tpe, right.tpe) match {
      case (_: BasicType, _: BasicType) => Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position)

      case (_: SymbolType, _: SymbolType) => comparisonFunction match {
        case ComparisonFunction.AreEqual =>
          // Because symbol values are interned, they can be compared by reference, using the equality operator.
          Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position)

        case ComparisonFunction.IsLessThan | ComparisonFunction.IsLessThanOrEqual =>
          reporter.error(IllegalSymbolComparison(position))
          Expression.Hole(BasicType.Boolean, position)
      }

      case _ => CallTransformation.multiFunctionCall(comparisonFunction.name, Vector(left, right), BasicType.Boolean, position)
    }
  }

}
