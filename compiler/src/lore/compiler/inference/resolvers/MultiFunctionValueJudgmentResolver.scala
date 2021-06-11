package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.feedback.TypingFeedback.{MultiFunctionCoercionContextExpected, MultiFunctionCoercionIllegalOutput}
import lore.compiler.inference.Inference.{Assignments, instantiate}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.Registry
import lore.compiler.types.FunctionType

object MultiFunctionValueJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionValue] {

  // TODO: This judgment should be properly resolved using backtracking. Much like the MultiFunctionHint, we can give
  //       multiple experimental bounds to the target inference variable and see which ones work. In addition, the
  //       target must DEPEND on this judgment (via a dummy inference variable), because this judgment changes the
  //       bounds of the target. Currently, the judgment depends on the target, which is incorrect. The idea there was
  //       that the target needs to be fully typed before we can properly type it as a function.
  //       Alternative: Instead of backtracking, we could also involve the typing judgment in a cycle with the target.
  //       If we have a judgment `target <- mf as function` with the judgment having `iv1` as the dummy variable, we
  //       would add `target ~> iv1` and `iv1 ~> target` to the influence graph. The MultiFunctionValue judgment would
  //       then only be resolved via cycle resolution. This would ensure that judgments like MultiFunctionHints are
  //       resolved before the MultiFunctionValue judgments, which are crucial to properly type the target inference
  //       variable. If that doesn't pan out, we can still implement backtracking.

  override def forwards(
    judgment: TypingJudgment.MultiFunctionValue,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // The resolution of a MultiFunctionValue judgment mostly depends on `target` already having some sort of
    // context with which to extract the fitting function type from the multi-function. Some inference variables in the
    // context may still be without a definition, which is why we instantiate them as Any.
    // TODO: Handle the case that `target` can't even be instantiated. The error should say something like "more
    //       context needed" OR we could attempt to type the multi-function as its root type. This only works if
    //       there is only one root function, of course.
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
