package lore.compiler.typing

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, LambdaReporter, Reporter}
import lore.compiler.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.inference.InferenceBounds.{BoundType, ensureLowerBound, ensureUpperBound}
import lore.compiler.inference.matchers.{Matchers, SubtypingMatcher}
import lore.compiler.inference.resolvers.SubtypesJudgmentResolver.ensureSubtypes
import lore.compiler.inference.{Inference, InferenceBounds, InferenceVariable, TypingJudgment, Unification}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, Type}

// TODO (inference): This obviously needs a more descriptive name.
object Helpers {
  case class AssignmentFailed(iv: InferenceVariable, tpe: Type, assignments: Assignments, context: Expression) extends Feedback.Error(context) {
    override def message: String = s"The inference variable $iv cannot be assigned the type $tpe given the existing assignment ${InferenceVariable.bounds(iv, assignments)}."
  }

  /**
    * Assigns `tpe` to `iv` in the given assignments.
    *
    * TODO (inference): This uses the old bounds assignment style of [[InferenceBounds.narrowBounds]], but might not
    *                   need the lower/upper bounds split. We will have to see, in the future, how this form of
    *                   assignment can be simplified.
    */
  def assign(iv: InferenceVariable, tpe: Type, assignments: Assignments): Option[Assignments] = {
    val bounds = InferenceVariable.bounds(iv, assignments)

    if (bounds.lower <= tpe && tpe <= bounds.upper) {
      Some(assignments.updated(iv, InferenceBounds(iv, tpe, tpe)))
    } else {
      None
    }
  }

  /**
    * Unifies `t1` and `t2` such that `t1` is equal to `t2`, assigning inference variables accordingly.
    *
    * TODO (inference): This uses the old Unification object, so the code should be moved and simplified.
    */
  def unifyEquals(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    // TODO (inference): This is a temporary mock so that we can use the old Unification object.
    val context = TypingJudgment.Equals(t1, t2, Position.internal)

    // TODO (inference): The reporter has to suppress any generated errors, because we want `unifyEquals` to not leak
    //                   any errors.
    implicit val reporter: Reporter = new LambdaReporter(_ => { })

    Unification.unify(t1, t2, assignments, context)
  }

  /**
    * Unifies `t1` and `t2` such that `t1` is a subtype of `t2`, assigning inference variables accordingly.
    *
    * TODO (inference): This uses the old SubtypingMatcher, so the code should be moved and simplified.
    */
  def unifySubtypes(t1: Type, t2: Type, assignments: Assignments): Option[Assignments] = {
    // TODO (inference): This is a temporary mock so that we can use the old SubtypingMatcher.
    val context = TypingJudgment.Subtypes(t1, t2, Position.internal)

    // TODO (inference): The reporter has to suppress any generated errors, because we want `unifySubtypes` to not leak
    //                   any errors.
    implicit val reporter: Reporter = new LambdaReporter(_ => { })

    SubtypingMatcher.matchSubtype(SubtypesProcessor)(t1, t2, assignments, context)
  }

  private object SubtypesProcessor extends Matchers.Processor {
    override def processIv1(iv1: InferenceVariable, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureUpperBound(assignments, iv1, instantiateByBound(assignments, t2, BoundType.Upper), context).flatMap {
        assignments2 => unifySubtypes(instantiateByBound(assignments2, iv1, BoundType.Lower), t2, assignments2)
      }
    }

    override def processIv2(t1: Type, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, t1, BoundType.Lower), context).flatMap {
        assignments2 => unifySubtypes(t1, instantiateByBound(assignments2, iv2, BoundType.Upper), assignments2)
      }
    }

    override def processBoth(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, iv1, BoundType.Lower), context).flatMap {
        assignments2 => ensureUpperBound(assignments2, iv1, instantiateByBound(assignments2, iv2, BoundType.Upper), context)
      }
    }
  }

  // TODO (inference): This error needs to be moved, of course, but it's just a sort of temporary error that will allow
  //                   us to see how robust this system is, and then implement targeted solutions.
  case class IncompleteAssignments(tpe: Type, result: Type, assignments: Assignments, context: Expression) extends Feedback.Error(context) {
    override def message: String = s"The type $tpe cannot be instantiated given the current assignments. The result is $result."
  }

  /**
    * Instantiates all inference variables in `tpe` from the given assignments. The function reports an error if not
    * all inference variables can be instantiated.
    *
    * TODO (inference): This should rather return `None` if the type cannot be fully instantiated. This allows the
    *                   caller of `instantiate` to properly handle an instantiation failure.
    */
  def instantiate(tpe: Type, assignments: Assignments, context: Expression)(implicit reporter: Reporter): Type = {
    val result = Inference.instantiate(assignments, tpe, BoundType.Lower, (bounds, _) => {
      if (bounds.lower > BasicType.Nothing) bounds.lower
      else if (bounds.upper < BasicType.Any) bounds.upper
      else bounds.variable // This case is what separates the instantiation from `instantiateCandidateType`.
    })
    if (!Inference.isFullyInstantiated(result)) {
      reporter.error(IncompleteAssignments(tpe, result, assignments, context))
    }
    result
  }

  /**
    * Instantiates the type of `expression` with all inference variables from the given assignments. The function
    * reports an error if not all inference variables can be instantiated.
    */
  def instantiate(expression: Expression, assignments: Assignments)(implicit reporter: Reporter): Type = instantiate(expression.tpe, assignments, expression)

  def instantiate(expressions: Vector[Expression], assignments: Assignments)(implicit reporter: Reporter): Vector[Type] = expressions.map(instantiate(_, assignments))

  /**
    * Guesses the best instantiation for `tpe` from the given assignments. This can be used to preprocess types for
    * error reporting.
    */
  def instantiateCandidate(tpe: Type, assignments: Assignments): Type = Inference.instantiateCandidateType(assignments, tpe)
}
