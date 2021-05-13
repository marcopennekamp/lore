package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.{illegalBackwards, illegalForwards}
import lore.compiler.phases.transformation.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.{FunctionType, Type}

/**
  * A judgment resolver resolves a specific typing judgment in either the forwards or backwards directions, or both.
  *
  * The forwards/backwards functions without a `remainingJudgments` parameter are known as <b>flat</b> resolution
  * functions. Such a function simply consumes the given typing judgment without producing or consuming any other
  * typing judgments.
  *
  * For each direction, you should override either the flat or the regular resolution function, but not both. The flat
  * function won't be invoked when the regular function is overridden.
  */
trait JudgmentResolver[A <: TypingJudgment] {

  def forwards(judgment: A, assignments: Assignments)(implicit registry: Registry): Compilation[Assignments] = illegalForwards(judgment)

  def backwards(judgment: A, assignments: Assignments)(implicit registry: Registry): Compilation[Assignments] = illegalBackwards(judgment)

  def forwards(
    judgment: A,
    assignments: Assignments,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = JudgmentResolver.flat(remainingJudgments)(forwards(judgment, assignments))

  def backwards(
    judgment: A,
    assignments: Assignments,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = JudgmentResolver.flat(remainingJudgments)(backwards(judgment, assignments))

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

  def flat(remainingJudgments: Vector[TypingJudgment])(result: Compilation[Assignments]): Compilation[JudgmentResolver.Result] = result.map((_, remainingJudgments))

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
    )(implicit registry: Registry): Compilation[Assignments] = throw new UnsupportedOperationException

    def nondirectional(
      judgment: A,
      assignments: Assignments,
      remainingJudgments: Vector[TypingJudgment],
    )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = JudgmentResolver.flat(remainingJudgments)(nondirectional(judgment, assignments))

    override final def forwards(
      judgment: A,
      assignments: Assignments,
      remainingJudgments: Vector[TypingJudgment]
    )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = nondirectional(judgment, assignments, remainingJudgments)

    override final def backwards(
      judgment: A,
      assignments: Assignments,
      remainingJudgments: Vector[TypingJudgment]
    )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = nondirectional(judgment, assignments, remainingJudgments)

    override final def forwards(judgment: A, assignments: Assignments)(implicit registry: Registry): Compilation[Assignments] = throw new UnsupportedOperationException

    override final def backwards(judgment: A, assignments: Assignments)(implicit registry: Registry): Compilation[Assignments] = throw new UnsupportedOperationException

  }

  // TODO: Move these errors somewhere else...

  case class CollectionExpected(actualType: Type, pos: Position) extends Error(pos) {
    override def message: String = s"Expected a collection at this position. Got a value of type $actualType."
  }

  case class EmptyFit(mf: MultiFunctionDefinition, inputType: Type)(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site has an empty fit. We cannot" +
      s" find a function of that name that would accept the given arguments with the type $inputType."
  }

  case class AmbiguousCall(
    mf: MultiFunctionDefinition, inputType: Type, min: Vector[FunctionDefinition],
  )(implicit callPosition: Position) extends Error(callPosition) {
    override def message: String = s"The multi-function call ${mf.name} at this site is ambiguous." +
      s" That is, we are finding too many functions that would accept the given arguments of type $inputType." +
      s" These are: ${min.mkString(", ")}."
  }

  object MultiFunctionCoercion {

    case class FunctionContextExpected(mf: MultiFunctionDefinition, targetType: Type, pos: Position) extends Error(pos) {
      override def message: String = s"A multi-function can only be coerced to a function type. The target type is" +
        s" currently inferred to be $targetType, which is not a function type. Most likely, the multi-function" +
        s" ${mf.name} cannot be used as a value in this context."
    }

    case class IllegalOutput(mf: MultiFunctionDefinition, expectedFunction: FunctionType, actualFunction: FunctionType, pos: Position) extends Error(pos) {
      override def message: String = s"While coercing the multi-function ${mf.name} to a function, the following function type" +
        s" was expected: $expectedFunction. The actual function type inferred via dispatch is $actualFunction. The" +
        s" multi-function cannot be coerced to the expected function type because the output types are incompatible."
    }

  }

  /**
    * @param remainingJudgments The remaining judgments <b>excluding</b> the one being resolved with this function.
    */
  def resolve(
    judgment: TypingJudgment,
    direction: ResolutionDirection,
    assignments: Inference.Assignments,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = {
    // Casting to JudgmentResolver[TypingJudgment] is sadly necessary, because the type system doesn't see that the
    // resolver's type corresponds exactly to the typing judgment's type.
    val resolver = (judgment match {
      case _: TypingJudgment.Equals => EqualsJudgmentResolver
      case _: TypingJudgment.Subtypes => SubtypesJudgmentResolver
      case _: TypingJudgment.Assign => AssignJudgmentResolver
      case _: TypingJudgment.LeastUpperBound => LeastUpperBoundJudgmentResolver
      case _: TypingJudgment.MemberAccess => MemberAccessJudgmentResolver
      case _: TypingJudgment.ElementType => ElementTypeJudgmentResolver
      case _: TypingJudgment.MultiFunctionCall => MultiFunctionCallJudgmentResolver
      case _: TypingJudgment.MultiFunctionValue => MultiFunctionValueJudgmentResolver
      case _: TypingJudgment.MostSpecific => ??? // TODO: Implement? (Probably throw away if MultiFunctionHint works.)
      case _: TypingJudgment.Conjunction => ??? // TODO: Implement? (Probably throw away if MultiFunctionHint works.)
      case _: TypingJudgment.MultiFunctionHint => MultiFunctionHintJudgmentResolver
    }).asInstanceOf[JudgmentResolver[TypingJudgment]]

    direction match {
      case ResolutionDirection.Forwards => resolver.forwards(judgment, assignments, remainingJudgments)
      case ResolutionDirection.Backwards => resolver.backwards(judgment, assignments, remainingJudgments)
    }
  }

}
