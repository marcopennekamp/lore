package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core._
import lore.compiler.phases.transformation.ExpressionBuilder.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.ExpressionTransformationVisitor.CollectionExpected
import lore.compiler.phases.typing.inference.Inference.{Assignments, instantiate, instantiateByBound}
import lore.compiler.phases.typing.inference.InferenceBounds.{BoundType, ensureBound, narrowBounds, overrideBounds}
import lore.compiler.phases.typing.inference.InferenceVariable.{isDefined, isDefinedAt}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.members.Member
import lore.compiler.types._

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

    // Simplify first so that we throw fewer judgments at the fixed-point algorithm.
    simplify(judgments).flatMap { case Simplification(complexJudgments, startingAssignments) =>
      step(startingAssignments, complexJudgments.reverse).flatMap { finalAssignments =>
        // TODO: Do we have to manually ensure that all inference variables contained in any judgment are assigned at least
        //       a candidate type? If this is not the case for some judgment list, type inference is not complete!
        //        - This is especially relevant now that member access has become more "soft", which might lead to some
        //          target inference variables to receive no bound at all. (Though that would probably just lead to a
        //          "member not found" compilation error during the final checks.)

        // Once all inference variables have been instantiated, make another pass over all judgments to check equality
        // and subtyping constraints.
        judgments.map(check(finalAssignments)).simultaneous.map(_ => finalAssignments)
      }
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
    * Takes a single step in the inference process by applying all judgments to the given assignment map. If the
    * assignments change, another step is initiated. Otherwise, the final assignments are returned.
    *
    * TODO: Terminate if we go 1000 iterations deep or detect cycles in another manner.
    */
  private def step(assignments: Assignments, judgments: Vector[TypingJudgment])(implicit registry: Registry): Compilation[Assignments] = {
    val compilation = judgments.foldLeft(Compilation.succeed(assignments)) {
      case (compilation, judgment) => compilation.flatMap(resolve(_, judgment))
    }

    compilation.flatMap { assignments2 =>
      if (assignments != assignments2) {
        println("Iteration result:")
        assignments2.foreach(println)
        println()
        step(assignments2, judgments)
      } else {
        Compilation.succeed(assignments2)
      }
    }
  }

  /**
    * Note: Resolution must be repeatable so that the fixed-point algorithm can work. Crucially, if a judgment is
    * resolved given assignments A and all the judgment's aspects have been incorporated into A, `resolve` should lead
    * to A and not another assignments map B or a compilation error, no matter how often the judgment is re-applied.
    */
  private def resolve(assignments: Assignments, judgment: TypingJudgment)(implicit registry: Registry): Compilation[Assignments] = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      Unification.unify(assignments, t1, t2, judgment)

    case TypingJudgment.Subtypes(t1, t2, _) =>
      TypeMatcher.ensureBoundsIfDefined(assignments, t2, t1, BoundType.Upper, judgment).flatMap(
        TypeMatcher.ensureBoundsIfDefined(_, t1, t2, BoundType.Lower, judgment),
      )

    case TypingJudgment.Assign(target, source, _) =>
      TypeMatcher.narrowBounds(assignments, source, target, judgment)

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
      def resolveAt(boundType: BoundType)(innerAssignments: Assignments) = {
        if (Inference.variables(source).forall(isDefinedAt(innerAssignments, _, boundType))) {
          val instantiatedSource = instantiateByBound(innerAssignments, source, boundType)
          instantiatedSource.members.get(name) match {
            case Some(member) => ensureBound(innerAssignments, target, member.tpe, boundType, judgment)
            case None => innerAssignments.compiled
          }
        } else innerAssignments.compiled
      }

      def resolveBackwards(innerAssignments: Assignments) = {
        if (Inference.variables(target).forall(isDefinedAt(innerAssignments, _, BoundType.Upper))) {
          // TODO: We need to merge shape types for this to work for two or more member accesses.
          val shape = ShapeType(name -> instantiateByBound(innerAssignments, target, BoundType.Upper))
          TypeMatcher.matchAll(InferenceBounds.ensureBound)(innerAssignments, shape, source, BoundType.Upper, judgment)
        } else innerAssignments.compiled
      }

      resolveAt(BoundType.Lower)(assignments).flatMap(resolveAt(BoundType.Upper)).flatMap(resolveBackwards)

    case TypingJudgment.ElementType(target, collection, position) =>
      if (Inference.variables(collection).forall(isDefined(assignments, _))) {
        // Much like member types, the element type is definitely determined by the collection type and may only change
        // by overriding its bounds due to a changing collection type.
        val instantiatedCollection = instantiate(assignments, collection, _.candidateType)
        val elementType = instantiatedCollection match {
          case ListType(element) => Compilation.succeed(element)
          case MapType(key, value) => Compilation.succeed(ProductType(Vector(key, value)))
          case _ => Compilation.fail(CollectionExpected(instantiatedCollection, position))
        }
        elementType.map(tpe => overrideBounds(assignments, target, tpe, tpe))
      } else Compilation.succeed(assignments)

    case TypingJudgment.MultiFunctionCall(target, mf, arguments, position) =>
      if (arguments.flatMap(Inference.variables).forall(isDefined(assignments, _))) {
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
      } else Compilation.succeed(assignments)

    case TypingJudgment.MostSpecific(reference, judgments, _) =>
      ???
  }

  case class UnsatisfiedEquals(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :=: $t2 remains unsatisfied after type inference!"
  }

  case class UnsatisfiedSubtypes(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :<: $t2 remains unsatisfied after type inference!"
  }

  case class UnsatisfiedMemberAccess(judgment: TypingJudgment, target: Type, source: Type, member: Member) extends Error(judgment) {
    override def message: String = s"The member ${member.name} of type $source has the type ${member.tpe}. However," +
      s" during inference, the type $target was inferred. There is likely not enough type information in your code." +
      s" If you are sure that your code is correct, please create a bug report on Github. The type inference code is" +
      s" by no means perfect. Much obliged!"
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

    case TypingJudgment.MemberAccess(target, source, name, position) =>
      val instantiatedTarget = instantiate(assignments, target, _.candidateType)
      val instantiatedSource = instantiate(assignments, source, _.candidateType)
      instantiatedSource.member(name)(position).flatMap { member =>
        if (instantiatedTarget != member.tpe) {
          Compilation.fail(UnsatisfiedMemberAccess(judgment, instantiatedTarget, instantiatedSource, member))
        } else Verification.succeed
      }

    // TODO: Check that LUB-based subtyping holds?
    case _ => Verification.succeed
  }

}
