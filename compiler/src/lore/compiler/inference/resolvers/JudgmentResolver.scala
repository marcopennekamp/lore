package lore.compiler.inference.resolvers

import lore.compiler.core.CompilationException
import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceOrder.InfluenceGraph
import lore.compiler.inference.resolvers.JudgmentResolver.{illegalBackwards, illegalForwards}
import lore.compiler.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry

/**
  * A judgment resolver resolves a specific typing judgment in either the forwards or backwards directions, or both.
  *
  * The forwards/backwards functions without a `remainingJudgments` parameter are known as <b>flat</b> resolution
  * functions. Such a function simply consumes the given typing judgment without producing or consuming any other
  * typing judgments.
  *
  * For each direction, you should override either the flat or the regular resolution function, but not both. The flat
  * function won't be invoked when the regular function is overridden.
  *
  * All resolution functions evaluate to None if an error occurs.
  */
trait JudgmentResolver[A <: TypingJudgment] {

  def forwards(judgment: A, assignments: Assignments)(implicit registry: Registry, reporter: Reporter): Option[Assignments] = illegalForwards(judgment)

  def backwards(judgment: A, assignments: Assignments)(implicit registry: Registry, reporter: Reporter): Option[Assignments] = illegalBackwards(judgment)

  def forwards(
    judgment: A,
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
    JudgmentResolver.flat(remainingJudgments)(forwards(judgment, assignments))
  }

  def backwards(
    judgment: A,
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
    JudgmentResolver.flat(remainingJudgments)(backwards(judgment, assignments))
  }

}

object JudgmentResolver {

  sealed trait ResolutionDirection
  object ResolutionDirection {
    case object Forwards extends ResolutionDirection
    case object Backwards extends ResolutionDirection
  }

  /**
    * A judgment resolution result. The resulting typing judgments are used as the basis for the next resolution step.
    * This list will usually equal the remaining judgments, but can be further modified by the judgment resolution to
    * implement advanced techniques such as backtracking (allowing the resolver to recurse into another resolution
    * step) and rewriting one judgment into a set of new judgments.
    */
  type Result = (Assignments, Vector[TypingJudgment])

  def illegalForwards(judgment: TypingJudgment) = throw CompilationException(s"The judgment `$judgment` cannot be resolved forwards.")
  def illegalBackwards(judgment: TypingJudgment) = throw CompilationException(s"The judgment `$judgment` cannot be resolved backwards.")

  def flat(remainingJudgments: Vector[TypingJudgment])(result: Option[Assignments]): Option[JudgmentResolver.Result] = result.map((_, remainingJudgments))

  /**
    * A nondirectional judgment resolver does not distinguish between forwards and backwards resolution. It will
    * resolve either direction using the same approach.
    *
    * As with forwards/backwards, override either the flat or the regular nondirectional resolution function, but not
    * both.
    */
  trait Nondirectional[A <: TypingJudgment] extends JudgmentResolver[A] {

    def nondirectional(
      judgment: A,
      assignments: Assignments,
    )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = throw new UnsupportedOperationException

    def nondirectional(
      judgment: A,
      assignments: Assignments,
      influenceGraph: InfluenceGraph,
      remainingJudgments: Vector[TypingJudgment],
    )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
      JudgmentResolver.flat(remainingJudgments)(nondirectional(judgment, assignments))
    }

    override final def forwards(
      judgment: A,
      assignments: Assignments,
      influenceGraph: InfluenceGraph,
      remainingJudgments: Vector[TypingJudgment]
    )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
      nondirectional(judgment, assignments, influenceGraph, remainingJudgments)
    }

    override final def backwards(
      judgment: A,
      assignments: Assignments,
      influenceGraph: InfluenceGraph,
      remainingJudgments: Vector[TypingJudgment]
    )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
      nondirectional(judgment, assignments, influenceGraph, remainingJudgments)
    }

    override final def forwards(judgment: A, assignments: Assignments)(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
      throw new UnsupportedOperationException
    }

    override final def backwards(judgment: A, assignments: Assignments)(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
      throw new UnsupportedOperationException
    }

  }

  /**
    * Resolves the judgment. Evaluates to None if an error occurs.
    *
    * @param remainingJudgments The remaining judgments <b>excluding</b> the one being resolved with this function.
    */
  def resolve(
    judgment: TypingJudgment,
    direction: ResolutionDirection,
    assignments: Inference.Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
    // Casting to JudgmentResolver[TypingJudgment] is sadly necessary, because the type system doesn't see that the
    // resolver's type corresponds exactly to the typing judgment's type.
    val resolver = (judgment match {
      case _: TypingJudgment.Equals => EqualsJudgmentResolver
      case _: TypingJudgment.Subtypes => SubtypesJudgmentResolver
      case _: TypingJudgment.Assign => AssignJudgmentResolver
      case _: TypingJudgment.Fits => FitsJudgmentResolver
      case _: TypingJudgment.LeastUpperBound => LeastUpperBoundJudgmentResolver
      case _: TypingJudgment.MemberAccess => MemberAccessJudgmentResolver
      case _: TypingJudgment.ElementType => ElementTypeJudgmentResolver
      case _: TypingJudgment.MultiFunctionCall => MultiFunctionCallJudgmentResolver
      case _: TypingJudgment.MultiFunctionValue => MultiFunctionValueJudgmentResolver
      case _: TypingJudgment.MultiFunctionHint => MultiFunctionHintJudgmentResolver
    }).asInstanceOf[JudgmentResolver[TypingJudgment]]

    direction match {
      case ResolutionDirection.Forwards => resolver.forwards(judgment, assignments, influenceGraph, remainingJudgments)
      case ResolutionDirection.Backwards => resolver.backwards(judgment, assignments, influenceGraph, remainingJudgments)
    }
  }

}
