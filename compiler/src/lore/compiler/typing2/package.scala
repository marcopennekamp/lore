package lore.compiler

import lore.compiler.core.{CompilationException, UniqueKey}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.typed.Expression

package object typing2 {
  /**
    * For each untyped local variable (by unique key), this map records the actual typed [[LocalVariable]].
    *
    * We have to carry around this context during type checking because types of local variables are usually unknown
    * until the variable declaration has been type-checked.
    *
    * TODO (multi-import): Note that this context approach doesn't work with type variables. A type variable could be
    *   used in the same function multiple times. For example, when calling `lore.list.map` twice, the first
    *   instantiation of `A` is different than the second one. We can't just keep type variable assignments in a global
    *   map. The old algorithm circumvented this issue by replacing type variables with inference variables, which
    *   would be unique per usage. We likely won't need a global context for type variables anyway; the same
    *   information could be kept local when multi-function calls etc. are resolved. (Sort of: get initial type
    *   variable information into a map, then infer all untyped arguments, then finish the type variable assignments
    *   map, and then use that information to ultimately type the whole call and especially the output type.)
    */
  case class InferenceContext(
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
