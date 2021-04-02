package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.Type

object CallVerification {

  case class IllegalReturnType(call: Expression.Call, expectedTypes: Vector[Type]) extends Error(call) {
    override def message = s"Calling ${call.target} returns the illegal type ${call.tpe}.$expected"
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types: ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  /**
    * Ensures that the given call expression has the expected output type.
    */
  def ensureOutputType(expectedType: Type)(call: Expression.Call): Compilation[Expression] = {
    if (call.tpe == expectedType) call.compiled
    else Compilation.fail(IllegalReturnType(call, Vector(expectedType)))
  }

}
