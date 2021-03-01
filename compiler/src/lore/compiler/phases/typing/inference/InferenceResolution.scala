package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation.Verification
import lore.compiler.core._
import lore.compiler.phases.transformation.ExpressionBuilder.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.ExpressionTransformationVisitor.CollectionExpected
import lore.compiler.phases.typing.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.typing.inference.InferenceBounds.{BoundType, narrowBounds, overrideBounds}
import lore.compiler.phases.typing.inference.InferenceVariable.isDefined
import lore.compiler.semantics.Registry
import lore.compiler.types.{LeastUpperBound, ListType, MapType, ProductType, Type}

object InferenceResolution {

  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
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

      // The equality can be ensured in two ways: Either t1 is matched to t2, or t2 is matched to t1. If one match
      // operation fails, we can try the other direction. If BOTH directions fail, we have a problem. In the end, the
      // goal is to get the types to be unified.
      // A good example why we have to gracefully fail potential matches is the following. Suppose we have these
      // typing judgments processed in the given order:
      //    i1 :=: Int, i2 :<: Real, Int :<: i2, i2 :=: i1
      // If we assign i2 to i1 first, we will fail because the upper bound Int of i1 cannot be narrowed to Real, the
      // upper bound of i2. We might throw in the towel here, but we can still achieve the objective of the Equals
      // typing judgment by assigning i1 to i2, which will give i2 the lower and upper bound Int. By failing gracefully
      // and trying in the other direction, the ultimate goal of the Equals judgment is fulfilled.
      TypeMatcher.narrowBounds(assignments, t1, t2, judgment).recover {
        _ => TypeMatcher.narrowBounds(assignments, t2, t1, judgment)
      }

    case TypingJudgment.Subtypes(t1, t2, _) =>
      // A subtyping relationship t1 :<: t2 can inform both the upper bound of t1 as well as the lower bound of t2.
      TypeMatcher.ensureBoundsIfDefined(assignments, t2, t1, BoundType.Upper, judgment).flatMap(
        TypeMatcher.ensureBoundsIfDefined(_, t1, t2, BoundType.Lower, judgment),
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
      instantiate(assignments, source, BoundType.Upper).member(name)(judgment.position).map { member =>
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

  case class UnsatisfiedEquals(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :=: $t2 remains unsatisfied after type inference!"
  }

  case class UnsatisfiedSubtypes(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :<: $t2 remains unsatisfied after type inference!"
  }

  /**
    * Checks the given typing judgment with instantiated types. This ensures that type equality and subtyping
    * relationships actually hold with the inferred type assignments.
    */
  private def check(assignments: Assignments)(judgment: TypingJudgment): Verification = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      val it1 = instantiate(assignments, t1, BoundType.Upper)
      val it2 = instantiate(assignments, t2, BoundType.Upper)
      if (it1 != it2) {
        Compilation.fail(UnsatisfiedEquals(judgment, it1, it2))
      } else Verification.succeed

    case TypingJudgment.Subtypes(t1, t2, _) =>
      val it1 = instantiate(assignments, t1, BoundType.Upper)
      val it2 = instantiate(assignments, t2, BoundType.Upper)
      if (!(it1 <= it2)) {
        Compilation.fail(UnsatisfiedSubtypes(judgment, it1, it2))
      } else Verification.succeed

    case _ => Verification.succeed
  }

}
