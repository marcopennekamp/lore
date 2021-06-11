package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.feedback.TypingFeedback.{MultiFunctionCoercionContextExpected, MultiFunctionCoercionIllegalOutput}
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.Registry
import lore.compiler.types.FunctionType

object MultiFunctionValueJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionValue] {

  override def forwards(
    judgment: TypingJudgment.MultiFunctionValue,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // The resolution of a MultiFunctionValue judgment mostly depends on `target` already having some sort of context
    // with which to extract the fitting function type from the multi-function. Some inference variables in the context
    // may still be without a definition, which is why we instantiate them as Any.
    instantiateCandidateType(assignments, judgment.target) match {
      case expectedFunctionType: FunctionType =>
        MultiFunctionCallJudgmentResolver.resolveDispatch(judgment.mf, expectedFunctionType.inputTuple, judgment.position, assignments).flatMap { instance =>
          val actualFunctionType = instance.signature.functionType
          if (actualFunctionType.output <= expectedFunctionType.output) {
            narrowBounds(assignments, judgment.target, actualFunctionType, actualFunctionType, judgment)
          } else {
            Compilation.fail(MultiFunctionCoercionIllegalOutput(judgment.mf, expectedFunctionType, actualFunctionType, judgment))
          }
        }

      case candidateType => Compilation.fail(MultiFunctionCoercionContextExpected(judgment.mf, candidateType, judgment))
    }
  }

}
