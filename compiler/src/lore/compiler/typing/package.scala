package lore.compiler

import lore.compiler.core.{CompilationException, UniqueKey}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.types.Type

package object typing {
  /**
    * For each untyped local variable (by unique key), the inference context records the actual typed [[LocalVariable]].
    * The context also carries the expected `returnType` of the surrounding function, used to check `Return`
    * expressions.
    */
  case class InferenceContext(
    returnType: Type,
    localVariables: Map[UniqueKey, LocalVariable],
  ) {
    def withLocalVariable(variable: LocalVariable): InferenceContext = {
      if (localVariables.contains(variable.uniqueKey)) {
        throw CompilationException(s"The local variable ${variable.name} with unique key ${variable.uniqueKey} is" +
          s" already registered in the inference context.")
      }
      this.copy(localVariables = localVariables + (variable.uniqueKey -> variable))
    }
  }

  type InferenceResult = (Expression, InferenceContext)
  type InferenceResults = (Vector[Expression], InferenceContext)
}
