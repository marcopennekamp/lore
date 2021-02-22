package lore.compiler.phases.typing

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error, Errors, Position, Result}
import lore.compiler.phases.transformation.ExpressionBuilder.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.ExpressionTransformationVisitor.CollectionExpected
import lore.compiler.phases.typing.InferenceAssignments._
import lore.compiler.semantics.Registry
import lore.compiler.types._

class Inference(judgments: Vector[TypingJudgment])(implicit registry: Registry) {

  def inferTypes(): Compilation[Assignments] = {
    var assignments: Assignments = Map.empty
    var workingSet = judgments.toSet

    // TODO: We probably need to solve this problem using a fixed-point approach, where the algorithm applies all
    //       resolvable judgments with each step until there are no more changes to the assignments.
    //       For example, let's say we have an equality a :=: b and b is already defined as b(lower: Nothing, upper: X).
    //       So we define a as the same. But then b is later defined as b(lower: Y, upper: Y) through a more complicated
    //       judgment. Then we have to redefine a as a(lower: Y, upper: Y).
    // TODO: To optimize checking assignments equality (we want to stop if old assignments == new assignments), we
    //       could keep a changeset with each iteration of the algorithm that tracks bounds changes. If the changeset
    //       is empty, assignments haven't changed and the algorithm can terminate.

    while (workingSet.nonEmpty) {
      // We have to pick judgments which we can resolve right away.
      // TODO: We certainly won't resolve cyclical relationships between inference variables this way. The question
      //       is whether that's a problem or not. :)
      val resolvable = workingSet.filter(isResolvable(assignments, _))
      if (resolvable.isEmpty) {
        throw CompilationException(s"The type inference working set $workingSet cannot be reduced any further. This is likely due to a flaw in the type inference algorithm.")
      }

      val compilation = resolvable.toVector.foldLeft(Compilation.succeed(assignments)) {
        case (compilation, typingJudgment) => compilation.flatMap { assignments => resolve(assignments, typingJudgment) }
      }

      compilation match {
        case Result(newAssignments, _) =>
          assignments = newAssignments
        case Errors(_, _) =>
          return compilation
      }

      workingSet = workingSet -- resolvable
    }

    // Once all inference variables have been instantiated, make another pass over all judgments to check equality
    // and subtyping constraints.
    judgments.map(check(assignments)).simultaneous.map(_ => assignments)
  }

  /**
    * A judgment is resolvable if all inference variables in a "source" position have been defined.
    */
  private def isResolvable(assignments: Assignments, judgment: TypingJudgment): Boolean = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      // The Equals judgment is resolvable if either t1 or t2 contain no undefined inference variables. Even if the
      // other type contains multiple inference variables, the judgment can be resolved by assigning multiple variables.
      // TODO: Move the "isDefined" stuff into a helper function. Maybe rename to "hasVariables" or "hasInferenceVariables".
      Inference.variables(t1).forall(isDefined(assignments, _)) || Inference.variables(t2).forall(isDefined(assignments, _))

    case TypingJudgment.Subtypes(t1, t2, _) =>
      // Note that a subtyping relationship t1 :<: t2 where only t2 contains inference variables is still resolvable,
      // because the inference algorithm can set the lower bound of t2 to t1, which provides useful information.
      Inference.variables(t1).forall(isDefined(assignments, _)) || Inference.variables(t2).forall(isDefined(assignments, _))

    case judgment: TypingJudgment.Operation => judgment.operands.flatMap(Inference.variables).forall(isDefined(assignments, _))

    case TypingJudgment.MostSpecific(_, alternatives, _) =>
      alternatives.forall(isResolvable(assignments, _))
  }

  /**
    * Note: Resolution must be repeatable so that the fixed-point algorithm can work. Crucially, if a judgment is
    * resolved given assignments A and all the judgment's aspects have been incorporated into A, `resolve` should lead
    * to A and not another assignments map B or a compilation error, no matter how often the judgment is re-applied.
    */
  private def resolve(assignments: Assignments, judgment: TypingJudgment)(implicit registry: Registry): Compilation[Assignments] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      // TODO: If either type is not fully defined, we certainly want to apply the rule later once again to catch all
      //       relationships. This plays into the idea that the constraints need to be applied with a fixed-point
      //       algorithm. Note that "fully defined" means that the bounds have settled on their eventual result. This
      //       is in contrast to merely "defined" variables, which may still change their bounds.

      // The equality can be ensured in two ways: Either t1 is assigned to t2, or t2 is assigned to t1. If one of the
      // assignments fails, we can try the other direction. If BOTH directions fail, we have a problem. In the end, the
      // goal is to get the types to be unified.
      // A good example why we have to gracefully fail potential assignments is the following. Suppose we have these
      // typing judgments processed in the given order:
      //    i1 :=: Int, i2 :<: Real, Int :<: i2, i2 :=: i1
      // If we assign i2 to i1 first, we will fail because the upper bound Int of i1 cannot be narrowed to Real, the
      // upper bound of i2. We might throw in the towel here, but we can still achieve the objective of the Equals
      // typing judgment by assigning i1 to i2, which will give i2 the lower and upper bound Int. By failing gracefully
      // and trying in the other direction, the ultimate goal of the Equals judgment is fulfilled.
      correlateIfDefined(narrowBound)(assignments, t2, t1, Vector(BoundType.Lower, BoundType.Upper), judgment).recover {
        _ => correlateIfDefined(narrowBound)(assignments, t1, t2, Vector(BoundType.Lower, BoundType.Upper), judgment)
      }

    case TypingJudgment.Subtypes(t1, t2, _) =>
      // A subtyping relationship t1 :<: t2 can inform both the upper bound of t1 as well as the lower bound of t2.
      correlateIfDefined(ensureBound)(assignments, t2, t1, Vector(BoundType.Upper), judgment).flatMap(
        assignments2 => correlateIfDefined(ensureBound)(assignments2, t1, t2, Vector(BoundType.Lower), judgment)
      )

    case TypingJudgment.LeastUpperBound(target, types, _) =>
      // TODO: Is this a derivative judgment? If so, shouldn't we override the bounds? We need to find examples and
      //       counterexamples.
      // The least upper bound calculated from the given types has to be calculated from the lower bounds and the upper
      // bounds separately.
      val lowerLub = LeastUpperBound.leastUpperBound(types.map(instantiate(assignments, _, BoundType.Lower)))
      val upperLub = LeastUpperBound.leastUpperBound(types.map(instantiate(assignments, _, BoundType.Upper)))
      narrowBounds(assignments, target, lowerLub, upperLub, judgment)

    case TypingJudgment.MemberAccess(target, source, name, _) =>
      // TODO: If we apply the fixpoint method, we might run into the following issue:
      //       1. At first, the source's upper bound is type `{ a: A1 }`. So the member `a` is typed as `A1` in both
      //          upper bound and lower bound.
      //       2. Later, the algorithm revises the source's type to `{ a: A2 }`. The member should then be typed as
      //          `A2`, but the lower bound is already `A1`. Supposing A2 < A1, the type inference algorithm will
      //          result in a compilation error.
      //       One of two things are going wrong here: Either the assumptions about lower bounds below are wrong, or we
      //       will need some way to invalidate some bounds based on which bounds are changing. Crucially, we have a
      //       sort of dependency graph where target depends on source. If the source's bounds change, the target
      //       bounds should maybe be invalidated.

      // For member access, the upper bound of `source` is crucial. Because a subtype of the upper bound will always
      // contain a member of a subtype compared to the upper bound's member, we can be sure that the upper bound's
      // member type is always a feasible choice.
      // The `target` inference variable gets this type not only as its upper bound, but also as its lower bound. The
      // reasoning is simple: Because the member's parent may always have its upper bound as a run-time type, the
      // member's type at run-time may also be what's now the type of the member chosen through the upper bound of
      // `source`. If we set the lower bound to anything but the member's upper bound, the type inference algorithm
      // might illegally narrow the type too far down.
      // In particular, we want to avoid that a subtyping judgment member_type :<: A2 narrows the member's previously
      // decided upper bound from A1 to A2.
      // TODO: Also refer to the TODO above. We might give MemberAccess the ability to override target bounds.
      //       Alternatively, maybe we need to go back to the drawing board and divide typing judgments into two
      //       classes: Assignments (which may override target bounds) and "typings". Perhaps these two concepts are
      //       actually wholly different. Maybe we need to create a "derivation" graph for the "assignments" like
      //       MemberAccess and ElementType.

      instantiate(assignments, source, BoundType.Upper).member(name)(judgment.position).map { member =>
        //narrowBounds(assignments, target, member.tpe, member.tpe, judgment)
        // TODO: Overriding the bounds is an idea and still needs to be verified!
        overrideBounds(assignments, target, member.tpe, member.tpe)
      }

    case TypingJudgment.ElementType(target, collection, position) =>
      // Much like member types, the element type is definitely determined by the collection type and may only change
      // by overriding its bounds due to a changing collection type.
      val instantiatedCollection = instantiate(assignments, collection, BoundType.Upper)
      val elementType = instantiatedCollection match {
        case ListType(element) => Compilation.succeed(element)
        case MapType(key, value) => Compilation.succeed(ProductType(Vector(key, value)))
        case _ => Compilation.fail(CollectionExpected(instantiatedCollection, position))
      }
      elementType.map(tpe => overrideBounds(assignments, target, tpe, tpe))

    case TypingJudgment.MultiFunctionCall(target, mf, arguments, position) =>
      // TODO: Once we activate the "MostSpecific" typing judgment that will also be generated with each multi-function
      //       call, we will already have performed the dispatch using the constraint solver. There might be no need to
      //       instantiate the function, for example, if we take the bounds inferred for the type variables, instead.
      implicit val callPosition: Position = position
      val inputType = ProductType(arguments.map(tpe => instantiate(assignments, tpe, BoundType.Upper)))
      mf.min(inputType) match {
        case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType))
        case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min))
        case functionDefinition +: _ =>
          functionDefinition.instantiate(inputType).map { instance =>
            // TODO: Should we override bounds here? Find examples and counterexamples.
            val result = instance.signature.outputType
            overrideBounds(assignments, target, result, result)
          }
      }

    case TypingJudgment.MostSpecific(reference, judgments, _) =>
      ???
  }

  /**
    * Checks the given typing judgment as instantiated by the variables in the given assignments. This ensures that
    * type equality and subtyping relationships actually hold.
    */
  private def check(assignments: Assignments)(judgment: TypingJudgment): Verification = judgment match {
    case TypingJudgment.Equals(t1, t2, position) =>
      // TODO: Do we need to check upper and lower bounds or upper bounds only?
      if (instantiate(assignments, t1, BoundType.Upper) != instantiate(assignments, t2, BoundType.Upper)) {
        Compilation.fail() // TODO: Types not equal.
      } else Verification.succeed

    case TypingJudgment.Subtypes(t1, t2, position) =>
      // TODO: Do we need to check upper and lower bounds or upper bounds only?
      if (!(instantiate(assignments, t1, BoundType.Upper) <= instantiate(assignments, t2, BoundType.Upper))) {
        Compilation.fail() // TODO: Types aren't subtypes.
      } else Verification.succeed

    case _ => Verification.succeed
  }

}

object Inference {

  // TODO: Test and improve error messages.

  case class InvalidLowerBound(inferenceVariable: InferenceVariable, actual: Type, expected: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"Type error: $actual must be a supertype of $expected."
  }

  case class InvalidUpperBound(inferenceVariable: InferenceVariable, actual: Type, expected: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"Type error: $actual must be a subtype of $expected."
  }

  case class NonassignableType(source: Type, target: Type, context: TypingJudgment) extends Error(context.position) {
    override def message: String = s"bla blub" // TODO
  }

  def variables(tpe: Type): Set[InferenceVariable] = tpe match {
    case iv: InferenceVariable => Set(iv)
    case tv: TypeVariable => variables(tv.lowerBound) ++ variables(tv.upperBound)
    case SumType(types) => types.flatMap(variables)
    case IntersectionType(types) => types.flatMap(variables)
    case ProductType(elements) => elements.flatMap(variables).toSet
    case FunctionType(input, output) => variables(input) ++ variables(output)
    case ListType(element) => variables(element)
    case MapType(key, value) => variables(key) ++ variables(value)
    case ShapeType(properties) => properties.values.map(_.tpe).flatMap(variables).toSet
    case _: NamedType => Set.empty // TODO: Update when struct/trait types can have type parameters.
  }

  /**
    * Whether the given type contains no inference variables at all, which means that it can bypass type inference.
    */
  def isFullyInferred(tpe: Type): Boolean = variables(tpe).isEmpty

}
