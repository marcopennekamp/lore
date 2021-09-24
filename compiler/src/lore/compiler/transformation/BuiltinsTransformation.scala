package lore.compiler.transformation

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{ExpressionFeedback, MultiFunctionFeedback, Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.core.CoreMultiFunction
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.BinaryOperator
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.{BasicType, SymbolType, TupleType}

object BuiltinsTransformation {

  /**
    * Builds a multi-function call to a core multi-function such as `to_string`.
    */
  def multiFunctionCall(
    cmf: CoreMultiFunction,
    arguments: Vector[Expression],
    position: Position,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    cmf.mf match {
      case Some(mf) =>
        val inputType = TupleType(arguments.map(_.tpe))
        mf.dispatch(
          inputType,
          MultiFunctionFeedback.Dispatch.EmptyFit(mf, inputType, position),
          min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, inputType, min, position),
        ).map { instance =>
          // Even though we already checked during resolution that the core multi-function has the correct output type,
          // the user might define a more general core function that dispatch might lead to here. We thus have to check
          // once again that the output type is correct.
          val expression = Expression.Call(CallTarget.MultiFunction(mf), arguments, instance.signature.outputType, position)
          if (instance.signature.outputType </= cmf.outputType) {
            reporter.error(TypingFeedback.SubtypeExpected(instance.signature.outputType, cmf.outputType, expression))
          }
          expression
        }.getOrElse {
          Expression.Hole(cmf.outputType, position)
        }

      case None => Expression.Hole(cmf.outputType, position)
    }
  }

  /**
    * Transforms comparison operations (==, !=, <, <=, >, >=) into the proper expressions, invoking the standard
    * functions areEqual, isLessThan, and isLessThanOrEqual for complex comparisons. Results in an error if areEqual,
    * isLessThan, or isLessThanOrEqual doesn't return a boolean value.
    *
    * If an expression cannot be created, the function falls back to [[Expression.Hole]] with a Boolean type.
    */
  def transformComparison(
    cmf: CoreMultiFunction,
    basicOperator: BinaryOperator,
    left: Expression,
    right: Expression,
    position: Position,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    (left.tpe, right.tpe) match {
      case (t1: BasicType, t2: BasicType) if t1.isPrimitive && t2.isPrimitive =>
        Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position)

      case (_: SymbolType, _: SymbolType) => cmf match {
        case registry.core.equal =>
          // Because symbol values are interned, they can be compared by reference, using the equality operator.
          Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position)

        case registry.core.less_than | registry.core.less_than_equal =>
          reporter.error(ExpressionFeedback.IllegalSymbolComparison(position))
          Expression.Hole(BasicType.Boolean, position)

        case _ => throw CompilationException(s"The core multi-function ${cmf.name} is not a comparison function!")
      }

      case _ => multiFunctionCall(cmf, Vector(left, right), position)
    }
  }

}
