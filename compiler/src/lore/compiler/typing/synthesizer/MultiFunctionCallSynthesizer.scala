package lore.compiler.typing.synthesizer

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.{Checker, MultiFunctionCallChecker}

object MultiFunctionCallSynthesizer {

  /**
    * Infers a multi-function call of `mf` in `expression`, assigning the output type of the multi-function call to
    * `expression`'s type.
    */
  def infer(mf: MultiFunctionDefinition, expression: Expression.Call, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    MultiFunctionCallChecker.handle(mf, expression, None, assignments)
  }

}
