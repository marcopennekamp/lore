package lore.compiler.phases.transformation.inference

import lore.compiler.core.{Compilation, Error}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound, isFullyInstantiated}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, narrowBound}
import lore.compiler.phases.transformation.inference.InferenceVariable.{effectiveBounds, isDefined}
import lore.compiler.phases.transformation.inference.matchers.EqualityMatcher
import lore.compiler.types._

object Unification {

  case class ExpectedTypeEquality(t1: Type, t2: Type, context: TypingJudgment) extends Error(context) {
    override def message: String = s"The types $t1 and $t2 should be equal, but are not."
  }

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

  case class CannotUnifyLowerBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify lower bounds of $bounds1 and $bounds2." // TODO: Create a more user-friendly error.
  }

  case class CannotUnifyUpperBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify upper bounds of $bounds1 and $bounds2." // TODO: Create a more user-friendly error.
  }

  // TODO: Remove.
  case class ConflictingBounds(bounds1: InferenceBounds, bounds2: InferenceBounds, context: TypingJudgment) extends Error(context) {
    override def message: String = s"Type error: Cannot unify bounds $bounds1 and $bounds2 because the lower and upper bounds conflict." // TODO: Create a more user-friendly error.
  }

  /**
    * Unify the bounds of the two inference variables such that they have the same bounds which must also be a narrowed
    * version of their prior bounds.
    *
    * Only considers bounds given in `boundTypes`.
    */
  private def unifyInferenceVariables(assignments: Assignments, iv1: InferenceVariable, iv2: InferenceVariable, boundTypes: Vector[BoundType], context: TypingJudgment): Compilation[Assignments] = {
    val bounds1 = effectiveBounds(assignments, iv1)
    val bounds2 = effectiveBounds(assignments, iv2)

    val compilationLower = if (boundTypes.contains(BoundType.Lower)) {
      val lower = Type.maxOrEqual(bounds1.lowerOrNothing, bounds2.lowerOrNothing) match {
        case Some(t) => t
        case None => return Compilation.fail(CannotUnifyLowerBounds(bounds1, bounds2, context))
      }

      narrowBound(assignments, iv1, lower, BoundType.Lower, context)
        .flatMap(narrowBound(_, iv2, lower, BoundType.Lower, context))
    } else Compilation.succeed(assignments)

    compilationLower.flatMap { assignments2 =>
      val upper = Type.minOrEqual(bounds1.upperOrAny, bounds2.upperOrAny) match {
        case Some(t) => t
        case None => return Compilation.fail(CannotUnifyUpperBounds(bounds1, bounds2, context))
      }

      narrowBound(assignments2, iv1, upper, BoundType.Upper, context)
        .flatMap(narrowBound(_, iv2, upper, BoundType.Upper, context))
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
      if (isDefined(assignments2, iv) && !isFullyInstantiated(tpe)) {
        narrowTpeByBound(assignments2, BoundType.Lower).flatMap(narrowTpeByBound(_, BoundType.Upper))
      } else Compilation.succeed(assignments2)
    }
  }

}
