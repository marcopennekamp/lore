package lore.compiler.phases.transformation

import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.DispatchFeedback.{AmbiguousCall, EmptyFit}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.TupleType

object CallTransformation {

  /**
    * Builds a simple multi-function call without inference. Argument types may not contain inference variables.
    */
  def multiFunctionCall(
    functionName: String,
    arguments: Vector[Expression],
    position: Position,
  )(implicit registry: Registry): Compilation[Expression.Call] = {
    registry.resolveMultiFunction(functionName, position).flatMap { mf =>
      val inputType = TupleType(arguments.map(_.tpe))
      mf.dispatch(inputType, EmptyFit(mf, inputType, position), min => AmbiguousCall(mf, inputType, min, position)).map {
        instance => Expression.Call(CallTarget.MultiFunction(mf), arguments, instance.signature.outputType, position)
      }
    }
  }

}
