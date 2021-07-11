package lore.compiler.inference

import lore.compiler.feedback.Reporter
import lore.compiler.feedback.TypingFeedback.EqualTypesExpected
import lore.compiler.inference.Inference.{Assignments, instantiateByBound, isFullyInstantiated}
import lore.compiler.inference.InferenceBounds.{BoundType, narrowBound, narrowLowerBound, narrowUpperBound}
import lore.compiler.inference.matchers.{EqualityMatcher, Matchers}
import lore.compiler.types._

object Unification {

  def unify(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
    unify(t1, t2, assignments, Vector(BoundType.Lower, BoundType.Upper), context)
  }

  def unify(t1: Type, t2: Type, assignments: Assignments, boundTypes: Vector[BoundType], context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
    EqualityMatcher.matchEquals(UnificationProcessor(boundTypes))(t1, t2, assignments, context)
  }

  private case class UnificationProcessor(boundTypes: Vector[BoundType]) extends Matchers.Processor {
    override def processIv1(iv1: InferenceVariable, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      unifyInferenceVariableWithType(assignments, iv1, t2, boundTypes, context)
    }

    override def processIv2(t1: Type, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      unifyInferenceVariableWithType(assignments, iv2, t1, boundTypes, context)
    }

    override def processBoth(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      unifyInferenceVariables(assignments, iv1, iv2, boundTypes, context)
    }
  }

  /**
    * Unify the bounds of the two inference variables such that they have the same bounds which must also be a narrowed
    * version of their prior bounds.
    *
    * Only considers bounds given in `boundTypes`.
    */
  private def unifyInferenceVariables(
    assignments: Assignments,
    iv1: InferenceVariable,
    iv2: InferenceVariable,
    boundTypes: Vector[BoundType],
    context: TypingJudgment,
  )(implicit reporter: Reporter): Option[Assignments] = {
    val bounds1 = InferenceVariable.bounds(iv1, assignments)
    val bounds2 = InferenceVariable.bounds(iv2, assignments)

    val assignmentsLower = if (boundTypes.contains(BoundType.Lower)) {
      val lower = Type.mostGeneral(Vector(bounds1.lower, bounds2.lower)) match {
        case Vector(t) => t
        case _ =>
          reporter.error(EqualTypesExpected(bounds1.lower, bounds2.lower, context))
          return None
      }

      narrowLowerBound(assignments, iv1, lower, context).flatMap(narrowLowerBound(_, iv2, lower, context))
    } else Some(assignments)

    assignmentsLower.flatMap { assignments2 =>
      val upper = Type.mostSpecific(Vector(bounds1.upper, bounds2.upper)) match {
        case Vector(t) => t
        case _ =>
          reporter.error(EqualTypesExpected(bounds1.upper, bounds2.upper, context))
          return None
      }

      narrowUpperBound(assignments2, iv1, upper, context).flatMap(narrowUpperBound(_, iv2, upper, context))
    }
  }

  /**
    * Unify the given inference variable with the given type. This might not only assign new bounds to the inference
    * variable, but also instantiate the inference variable and assign bounds to any variables contained in `tpe`.
    *
    * Only considers bounds given in `boundTypes`.
    *
    * That the second case (iv --> tpe) is necessary can be illustrated with an example. Take for example the following
    * judgments:
    *   - iv1 :=: (A, B)
    *   - iv1 :=: (iv2, iv3)
    *
    * The first judgment will be resolved first, producing the bounds `iv1((A, B), (A, B))`. Resolution of the second
    * judgment will ultimately lead to this function being called with `iv = iv1` and `tpe = (iv2, iv3)`. Both `iv` and
    * `tpe` will be instantiated at the requested bound types. If `instantiate(tpe) <= instantiate(iv)` (replace with
    * `>=` for the upper bound), the algorithm opportunistically attempts to narrow all inference variables in `tpe`
    * using the instantiated version of `iv`. If this is not the case, the algorithm attempts to narrow `iv` with the
    * instantiated version of `tpe`. So in the given example, because `(Nothing, Nothing) <= (A, B)`, `iv2` and `iv3`
    * will be narrowed to `A` and `B` respectively. The same applies to the upper bound given `(Any, Any) >= (A, B)`.
    */
  private def unifyInferenceVariableWithType(
    assignments: Assignments,
    iv: InferenceVariable,
    tpe: Type,
    boundTypes: Vector[BoundType],
    context: TypingJudgment,
  )(implicit reporter: Reporter): Option[Assignments] = {
    def narrowByBound(assignments: Assignments, boundType: BoundType) = {
      if (boundTypes.contains(boundType)) {
        val instantiatedIv = instantiateByBound(assignments, iv, boundType)
        val instantiatedTpe = instantiateByBound(assignments, tpe, boundType)
        lazy val isTpeWider = boundType match {
          case BoundType.Lower => instantiatedTpe <= instantiatedIv
          case BoundType.Upper => instantiatedIv <= instantiatedTpe
        }
        if (!isFullyInstantiated(tpe) && isTpeWider) {
          unify(instantiatedIv, tpe, assignments, Vector(boundType), context)
        } else {
          narrowBound(assignments, iv, instantiatedTpe, boundType, context)
        }
      } else Some(assignments)
    }

    narrowByBound(assignments, BoundType.Lower).flatMap(narrowByBound(_, BoundType.Upper))
  }

}
