package lore.compiler.phases.transformation.inference

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound, isFullyInstantiated}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, narrowBound, narrowLowerBound, narrowUpperBound}
import lore.compiler.phases.transformation.inference.InferenceErrors.EqualTypesExpected
import lore.compiler.phases.transformation.inference.InferenceVariable.{effectiveBounds, isDefined}
import lore.compiler.phases.transformation.inference.matchers.EqualityMatcher
import lore.compiler.types._

object Unification {

  def unify(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment): Compilation[Assignments] = {
    unify(t1, t2, assignments, Vector(BoundType.Lower, BoundType.Upper), context)
  }

  def unify(t1: Type, t2: Type, assignments: Assignments, boundTypes: Vector[BoundType], context: TypingJudgment): Compilation[Assignments] = {
    EqualityMatcher.matchEquals(
      (iv1, t2, assignments, context) => unifyInferenceVariableWithType(assignments, iv1, t2, boundTypes, context),
      (t1, iv2, assignments, context) => unifyInferenceVariableWithType(assignments, iv2, t1, boundTypes, context),
      (iv1, iv2, assignments, context) => unifyInferenceVariables(assignments, iv1, iv2, boundTypes, context)
    )(t1, t2, assignments, context)
  }

  /**
    * Unify the bounds of the two inference variables such that they have the same bounds which must also be a narrowed
    * version of their prior bounds.
    *
    * Only considers bounds given in `boundTypes`.
    */
  private def unifyInferenceVariables(assignments: Assignments, iv1: InferenceVariable, iv2: InferenceVariable, boundTypes: Vector[BoundType], context: TypingJudgment): Compilation[Assignments] = {
    val bounds1 = effectiveBounds(iv1, assignments)
    val bounds2 = effectiveBounds(iv2, assignments)

    val compilationLower = if (boundTypes.contains(BoundType.Lower)) {
      val lower = Type.maxOrEqual(bounds1.lowerOrNothing, bounds2.lowerOrNothing) match {
        case Some(t) => t
        case None => return Compilation.fail(EqualTypesExpected(bounds1.lowerOrNothing, bounds2.lowerOrNothing, context))
      }

      narrowLowerBound(assignments, iv1, lower, context).flatMap(narrowLowerBound(_, iv2, lower, context))
    } else Compilation.succeed(assignments)

    compilationLower.flatMap { assignments2 =>
      val upper = Type.minOrEqual(bounds1.upperOrAny, bounds2.upperOrAny) match {
        case Some(t) => t
        case None => return Compilation.fail(EqualTypesExpected(bounds1.upperOrAny, bounds2.upperOrAny, context))
      }

      narrowUpperBound(assignments2, iv1, upper, context).flatMap(narrowUpperBound(_, iv2, upper, context))
    }
  }

  /**
    * Unify the given inference variable with the given type. This might not only assign new bounds to the inference
    * variable, but also instantiate the inference variable and assign bounds to any variables contained in `tpe`.
    *
    * Only considers bounds given in `boundTypes`.
    */
  private def unifyInferenceVariableWithType(assignments: Assignments, iv: InferenceVariable, tpe: Type, boundTypes: Vector[BoundType], context: TypingJudgment): Compilation[Assignments] = {
    // Narrows `iv` to an instantiated version of `tpe` on the given bound type.
    def narrowIvByBound(assignments: Assignments, boundType: BoundType) = {
      lazy val instantiatedType = instantiateByBound(assignments, tpe, boundType)
      if (boundTypes.contains(boundType) && isFullyInstantiated(instantiatedType)) {
        narrowBound(assignments, iv, instantiatedType, boundType, context)
      } else Compilation.succeed(assignments)
    }

    // Narrows all inference variables in `tpe` to an instantiated version of `iv` on the given bound type.
    def narrowTpeByBound(assignments: Assignments, boundType: BoundType) = {
      lazy val instantiatedIv = instantiateByBound(assignments, iv, boundType)
      if (boundTypes.contains(boundType) && isFullyInstantiated(instantiatedIv)) {
        unify(instantiatedIv, tpe, assignments, Vector(boundType), context)
      } else Compilation.succeed(assignments)
    }

    narrowIvByBound(assignments, BoundType.Lower).flatMap(narrowIvByBound(_, BoundType.Upper)).flatMap { assignments2 =>
      if (isDefined(iv, assignments2) && !isFullyInstantiated(tpe)) {
        narrowTpeByBound(assignments2, BoundType.Lower).flatMap(narrowTpeByBound(_, BoundType.Upper))
      } else Compilation.succeed(assignments2)
    }
  }

}
