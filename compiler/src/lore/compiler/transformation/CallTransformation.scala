package lore.compiler.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.inference.InferenceVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.CallTarget

object CallTransformation {

  /**
    * Builds a function value call from the given target and arguments.
    *
    * TODO (inference): We can probably inline this.
    */
  def valueCall(target: Expression, arguments: Vector[Expression], position: Position)(implicit reporter: Reporter): Expression.Call = {
    Expression.Call(CallTarget.Value(target), arguments, new InferenceVariable, position)
  }

}
