package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation.Verification
import lore.compiler.core._
import lore.compiler.phases.transformation.ExpressionBuilder.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.ExpressionTransformationVisitor.CollectionExpected
import lore.compiler.phases.typing.inference.Inference.{Assignments, instantiate, instantiateByBound}
import lore.compiler.phases.typing.inference.InferenceBounds.{BoundType, narrowBounds, overrideBounds}
import lore.compiler.phases.typing.inference.InferenceVariable.isDefined
import lore.compiler.semantics.Registry
import lore.compiler.types.{LeastUpperBound, ListType, MapType, ProductType, Type}

object InferenceResolution {

  def infer(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    // TODO: We probably need to solve this problem using a fixed-point approach, where the algorithm applies all
    //       resolvable judgments with each step until there are no more changes to the assignments.
    //       For example, let's say we have an equality a :=: b and b is already defined as b(lower: Nothing, upper: X).
    //       So we define a as the same. But then b is later defined as b(lower: Y, upper: Y) through a more complicated
    //       judgment. Then we have to redefine a as a(lower: Y, upper: Y).
    // TODO: To optimize checking assignments equality (we want to stop if old assignments == new assignments), we
    //       could keep a changeset with each iteration of the algorithm that tracks bounds changes. If the changeset
    //       is empty, assignments haven't changed and the algorithm can terminate.
    // TODO: Always sort resolvables so that the algorithm remains predictable throughout compilation passes. A fixed
    //       order isn't important when the algorithm is perfectly bug-free. But if the algorithm has a bug (which is
    //       and will remain likely due to its complexity), a fixed judgment order will make reproducing errors much
    //       easier.

    // Simplify first so that we
    simplify(judgments).flatMap { simplification =>
      var assignments = simplification.assignments
      var workingSet = simplification.complexJudgments.toSet

      while (workingSet.nonEmpty) {
        // We have to pick judgments which we can resolve right away.
        val resolvable = workingSet.filter(isResolvable(assignments, _))
        if (resolvable.isEmpty) {
          throw CompilationException(s"The type inference working set $workingSet cannot be reduced any further. This is likely due to a flaw in the type inference algorithm.")
        }

        //println(s"Resolvable: $resolvable")

        val compilation = resolvable.toVector.foldLeft(Compilation.succeed(assignments)) {
          case (compilation, typingJudgment) => compilation.flatMap { assignments => resolve(assignments, typingJudgment) }
        }

        compilation match {
          case Result(newAssignments, _) =>
            assignments = newAssignments
          //println(s"New assignments:")
          //newAssignments.values.toVector.sortBy(_.variable.actualName).foreach(println)
          //println()
          case Errors(_, _) =>
            //println(s"Failed!")
            //println()
            return compilation
        }

        workingSet = workingSet -- resolvable
      }

      // TODO: Do we have to manually ensure that all inference variables contained in any judgment are assigned at least
      //       a candidate type? If this is not the case for some judgment list, type inference is not complete!

      // Once all inference variables have been instantiated, make another pass over all judgments to check equality
      // and subtyping constraints.
      judgments.map(check(assignments)).simultaneous.map(_ => assignments)
    }
  }

  case class Simplification(complexJudgments: Vector[TypingJudgment], assignments: Assignments)

  /**
    * Simplifies a given list of judgments by checking, applying, and removing the judgments which do not depend on any
    * inference variables. The resulting assignment map can be viewed as a starting point for complex inference.
    */
  private def simplify(judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Simplification] = {
    val (simpleJudgments, complexJudgments) = judgments.partition(TypingJudgment.isSimple)

    val compilation = simpleJudgments.foldLeft(Compilation.succeed(Map.empty: Assignments)) {
      case (compilation, judgment) => compilation.flatMap(assignments => InferenceResolution.resolve(assignments, judgment))
    }

    compilation.map { assignments =>
      if (simpleJudgments.nonEmpty) {
        println("Simplified the following judgments:")
        simpleJudgments.foreach(println)
        println()
        println("Resulting in these starting assignments:")
        assignments.values.foreach(println)
        println()
      }

      Simplification(complexJudgments, assignments)
    }
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
      // TODO: Shouldn't either case have variables be defined at lower/upper bounds?
      Inference.variables(t1).forall(isDefined(assignments, _)) || Inference.variables(t2).forall(isDefined(assignments, _))

    case TypingJudgment.Assign(_, source, _) => Inference.variables(source).forall(isDefined(assignments, _))

    case TypingJudgment.LeastUpperBound(target, types, _) =>
      types.flatMap(Inference.variables).forall(isDefined(assignments, _)) || Inference.variables(target).forall(isDefined(assignments, _))

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
      Unification.unify(assignments, t1, t2, judgment)

    case TypingJudgment.Subtypes(t1, t2, _) =>
      // A subtyping relationship t1 :<: t2 can inform both the upper bound of t1 as well as the lower bound of t2:
      //  - In the upper-bound case, we instantiate t2's inference variables with their upper bounds. Consider the
      //    following example: iv1 :<: iv2(Int, Real). Because iv2 can at most be Real, iv1's domain must also be
      //    restricted to be at most Real. If we gave iv1 the upper bound Int, we might later type iv2 as Real and iv1
      //    would have an upper bound that's too narrow.
      //  - The lower-bound case is the dual to the upper-bound case. Given iv2(Int, Real) :<: iv1, we must assign to
      //    iv1 the lower bound Int. If we gave iv1 the lower bound Real, we might later type iv2 as Int and iv1 would
      //    have a lower bound that's too narrow.
      TypeMatcher.ensureBoundsIfDefined(assignments, t2, t1, BoundType.Upper, judgment).flatMap(
        TypeMatcher.ensureBoundsIfDefined(_, t1, t2, BoundType.Lower, judgment),
      )

    case TypingJudgment.Assign(target, source, _) => TypeMatcher.narrowBounds(assignments, source, target, judgment)

    case TypingJudgment.LeastUpperBound(target, types, _) =>
      val lubApplied = if (types.flatMap(Inference.variables).forall(isDefined(assignments, _))) {
        // The least upper bound calculated from the given types has to be calculated from the lower bounds and the upper
        // bounds separately.
        val lowerLub = LeastUpperBound.leastUpperBound(types.map(instantiateByBound(assignments, _, BoundType.Lower)))
        val upperLub = LeastUpperBound.leastUpperBound(types.map(instantiateByBound(assignments, _, BoundType.Upper)))
        narrowBounds(assignments, target, lowerLub, upperLub, judgment)
      } else Compilation.succeed(assignments)

      lubApplied.flatMap { assignments2 =>
        // TODO: Implement the target -> types direction.
        Compilation.succeed(assignments2)
      }

    case TypingJudgment.MemberAccess(target, source, name, _) =>
      // For member access, the candidate type of `source` is crucial.
      // TODO: Rewrite this:
      // Because a subtype of the upper bound will always
      // contain a member of a subtype compared to the upper bound's member, we can be sure that the upper bound's
      // member type is always a feasible choice.
      // The `target` inference variable gets this type not only as its upper bound, but also as its lower bound. The
      // reasoning is simple: Because the member's parent may always have its upper bound as a run-time type, the
      // member's type at run-time may also be what's now the type of the member chosen through the upper bound of
      // `source`. If we set the lower bound to anything but the member's upper bound, the type inference algorithm
      // might illegally narrow the type too far down.
      // In particular, we want to avoid that a subtyping judgment member_type :<: A2 narrows the member's previously
      // decided upper bound from A1 to A2.
      // TODO: When any inference variable bounds change in the source type, the member access should be re-evaluated
      //       right away (in the fixpoint algorithm view). We might need a dependency graph, generally, that allows us
      //       to follow the right "flow" in respect to which inference variables should be computed first.
      instantiate(assignments, source, _.candidateType).member(name)(judgment.position).map { member =>
        overrideBounds(assignments, target, member.tpe, member.tpe)
      }

    case TypingJudgment.ElementType(target, collection, position) =>
      // Much like member types, the element type is definitely determined by the collection type and may only change
      // by overriding its bounds due to a changing collection type.
      val instantiatedCollection = instantiate(assignments, collection, _.candidateType)
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
      val inputType = ProductType(arguments.map(tpe => instantiate(assignments, tpe, _.candidateType)))
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
      val it1 = instantiate(assignments, t1, _.candidateType)
      val it2 = instantiate(assignments, t2, _.candidateType)
      if (it1 != it2) {
        Compilation.fail(UnsatisfiedEquals(judgment, it1, it2))
      } else Verification.succeed

    case TypingJudgment.Subtypes(t1, t2, _) =>
      val it1 = instantiate(assignments, t1, _.candidateType)
      val it2 = instantiate(assignments, t2, _.candidateType)
      if (!(it1 <= it2)) {
        Compilation.fail(UnsatisfiedSubtypes(judgment, it1, it2))
      } else Verification.succeed

    // TODO: Check that LUB-based subtyping holds?
    case _ => Verification.succeed
  }

}
