package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.{MultiFunctionFeedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.core.CoreMultiFunction
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.BinaryOperator
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.{BasicType, TupleType}

object BuiltinsTransformation {

  /**
    * Builds a multi-function call to a core multi-function such as `lore.core.to_string`.
    */
  def multiFunctionCall(
    cmf: CoreMultiFunction,
    arguments: Vector[Expression],
    position: Position,
  )(implicit reporter: Reporter): Expression = {
    cmf.mf match {
      case Some(mf) =>
        val inputType = TupleType(arguments.map(_.tpe))
        mf.dispatch(
          inputType,
          MultiFunctionFeedback.Dispatch.EmptyFit(mf, inputType, position),
          min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, inputType, min, position),
        ).map { instance =>
          // The specific instance's output type will be a subtype of the CMF's expected output type because the
          // instance is necessarily a specialization of the CMF. Output types are kept consistent by multi-function
          // constraints.
          Expression.Call(CallTarget.MultiFunction(mf), arguments, instance.signature.outputType, position)
        }.getOrElse {
          Expression.Hole(cmf.outputType, position)
        }

      case None => Expression.Hole(cmf.outputType, position)
    }
  }

  /**
    * Transforms comparison operations (==, !=, <, <=, >, >=) into the proper expressions, invoking the standard
    * functions `equal?`, `less_than?`, and `less_than_equal?` for complex comparisons. Results in an error if these
    * functions don't return a boolean value.
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
    // The following type combinations can be compared by specialized instructions: `(Int | Real, Int | Real)`,
    // `(Boolean, Boolean)` (equality only), `(String, String)`, and `(Symbol, Symbol)` (equality only). Boolean and
    // symbol order is included in the default implementation of `lore.core.less_than?`, but not available as a
    // specialized instruction.
    val hasSpecializedInstruction = (left.tpe, right.tpe) match {
      case (t1: BasicType, t2: BasicType) if t1.isNumeric && t2.isNumeric => true
      case (BasicType.Boolean, BasicType.Boolean) => cmf == registry.coreDefinitions.equal
      case (BasicType.String, BasicType.String) => true
      case (t1, t2) if t1.isSymbol && t2.isSymbol => cmf == registry.coreDefinitions.equal
      case _ => false
    }

    if (hasSpecializedInstruction) Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position)
    else multiFunctionCall(cmf, Vector(left, right), position)
  }

}
