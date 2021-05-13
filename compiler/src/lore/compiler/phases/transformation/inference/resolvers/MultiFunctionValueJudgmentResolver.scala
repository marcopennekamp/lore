package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.transformation.inference.InferenceBounds.narrowBounds
import lore.compiler.phases.transformation.inference.TypingJudgment
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.MultiFunctionCoercion
import lore.compiler.semantics.Registry
import lore.compiler.types.FunctionType

object MultiFunctionValueJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionValue] {

  override def forwards(
    judgment: TypingJudgment.MultiFunctionValue,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // The resolution of a MultiFunctionValue judgment mostly depends on `target` already having some sort of
    // context with which to extract the fitting function type from the multi-function.
    // TODO: Handle the case that `target` can't even be instantiated. The error should say something like "more
    //       context needed" OR we could attempt to type the multi-function as its root type. This only works if
    //       there is only one root function, of course.
    instantiate(assignments, judgment.target, _.candidateType) match {
      case expectedFunctionType: FunctionType =>
        // TODO: Handle upper and lower bounds separately.
        MultiFunctionCallJudgmentResolver.resolveDispatch(judgment.mf, expectedFunctionType.inputTuple, judgment.position, assignments).flatMap { instance =>
          val actualFunctionType = instance.signature.functionType
          if (actualFunctionType.output <= expectedFunctionType.output) {
            narrowBounds(assignments, judgment.target, actualFunctionType, actualFunctionType, judgment)
          } else {
            Compilation.fail(MultiFunctionCoercion.IllegalOutput(judgment.mf, expectedFunctionType, actualFunctionType, judgment.position))
          }
        }

      case candidateType => Compilation.fail(MultiFunctionCoercion.FunctionContextExpected(judgment.mf, candidateType, judgment.position))
    }
  }

}
