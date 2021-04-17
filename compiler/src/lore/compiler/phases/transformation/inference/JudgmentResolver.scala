package lore.compiler.phases.transformation.inference

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate, instantiateByBound}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, ensureBound, narrowBounds}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionInstance, MultiFunctionDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.types.HasMembers.MemberNotFound
import lore.compiler.types._

object JudgmentResolver {

  sealed trait ResolutionDirection
  object ResolutionDirection {
    case object Forwards extends ResolutionDirection
    case object Backwards extends ResolutionDirection
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

  def resolve(
    judgment: TypingJudgment,
    direction: ResolutionDirection,
    assignments: Inference.Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    def illegalBackwards = throw CompilationException(s"The judgment `$judgment` cannot be resolved backwards.")

    judgment match {
      case TypingJudgment.Equals(t1, t2, _) =>
        // The direction is unimportant as we rely on unification.
        Unification.unify(assignments, t1, t2, judgment)

      case TypingJudgment.Subtypes(t1, t2, _) => direction match {
        case ResolutionDirection.Forwards => TypeMatcher.ensureBounds(assignments, t2, t1, BoundType.Upper, judgment)
        case ResolutionDirection.Backwards => TypeMatcher.ensureBounds(assignments, t1, t2, BoundType.Lower, judgment)
      }

      case TypingJudgment.Assign(target, source, _) => direction match {
        case ResolutionDirection.Forwards => TypeMatcher.narrowBounds(assignments, source, target, judgment)
        case ResolutionDirection.Backwards => illegalBackwards
      }

      case TypingJudgment.LeastUpperBound(target, types, _) => direction match {
        case ResolutionDirection.Forwards =>
          // The least upper bound calculated from the given types has to be calculated from the lower bounds and the
          // upper bounds separately.
          val lowerLub = LeastUpperBound.leastUpperBound(types.map(instantiateByBound(assignments, _, BoundType.Lower)))
          val upperLub = LeastUpperBound.leastUpperBound(types.map(instantiateByBound(assignments, _, BoundType.Upper)))
          narrowBounds(assignments, target, lowerLub, upperLub, judgment)

        case ResolutionDirection.Backwards => ??? // TODO: Implement the target -> types (backwards) direction.
      }

      case TypingJudgment.MemberAccess(target, source, name, _) =>
        def memberAt(boundType: BoundType): Compilation[Option[Member]] = {
          if (InferenceVariable.isDefinedAt(assignments, source, boundType)) {
            instantiateByBound(assignments, source, boundType).member(name)(judgment.position).map(Some(_))
          } else None.compiled
        }

        direction match {
          case ResolutionDirection.Forwards =>
            // We definitely have to resolve the member here (as all inference variables in source are fully inferred), but
            // the condition that the member is present is slightly softer than one would expect. If EITHER the lower or
            // upper instance contains the member, that is enough. We know that the member exists within the source type in
            // general and can bound the target variable appropriately.
            memberAt(BoundType.Lower).flatMap { lowerMember =>
              memberAt(BoundType.Upper).flatMap { upperMember =>
                if (lowerMember.nonEmpty || upperMember.nonEmpty) {
                  val compilationLower = lowerMember match {
                    case Some(member) => ensureBound(assignments, target, member.tpe, BoundType.Lower, judgment)
                    case None => Compilation.succeed(assignments)
                  }

                  compilationLower.flatMap { assignments2 =>
                    upperMember match {
                      case Some(member) => ensureBound(assignments2, target, member.tpe, BoundType.Upper, judgment)
                      case None => Compilation.succeed(assignments2)
                    }
                  }
                } else {
                  Compilation.fail(MemberNotFound(name, Inference.instantiate(assignments, source, _.candidateType), judgment.position))
                }
              }
            }

          case ResolutionDirection.Backwards =>
            // TODO: Re-implement backwards inference:
            /*
            def resolveBackwards(innerAssignments: Assignments) = {
              if (Inference.variables(target).forall(isDefinedAt(innerAssignments, _, BoundType.Upper))) {
                // TODO: We need to merge shape types for this to work for two or more member accesses.
                val shape = ShapeType(name -> instantiateByBound(innerAssignments, target, BoundType.Upper))
                TypeMatcher.matchAll(InferenceBounds.ensureBound)(innerAssignments, shape, source, BoundType.Upper, judgment)
              } else innerAssignments.compiled
            }
            */
            ???
        }

      case TypingJudgment.ElementType(target, collection, position) => direction match {
        case ResolutionDirection.Forwards =>
          // TODO: Candidate type or separate lower and upper bounds?
          val instantiatedCollection = instantiate(assignments, collection, _.candidateType)
          val elementType = instantiatedCollection match {
            case ListType(element) => Compilation.succeed(element)
            case MapType(key, value) => Compilation.succeed(ProductType(Vector(key, value)))
            case _ => Compilation.fail(CollectionExpected(instantiatedCollection, position))
          }
          elementType.flatMap(tpe => narrowBounds(assignments, target, tpe, tpe, judgment))

        case ResolutionDirection.Backwards => illegalBackwards
      }

      case TypingJudgment.MultiFunctionCall(target, mf, arguments, position) => direction match {
        case ResolutionDirection.Forwards =>
          // TODO: Once we activate the "MostSpecific" typing judgment that will also be generated with each multi-function
          //       call, we will already have performed the dispatch using the constraint solver. There might be no need to
          //       instantiate the function, for example, if we take the bounds inferred for the type variables, instead.
          // TODO: Handle upper and lower bounds separately.
          resolveDispatch(mf, ProductType(arguments), position, assignments).flatMap { instance =>
            val result = instance.signature.outputType
            narrowBounds(assignments, target, result, result, judgment)
          }

        case ResolutionDirection.Backwards => illegalBackwards
      }

      case TypingJudgment.MultiFunctionValue(target, mf, position) => direction match {
        case ResolutionDirection.Forwards =>
          // The resolution of a MultiFunctionValue judgment mostly depends on `target` already having some sort of
          // context with which to extract the fitting function type from the multi-function.
          // TODO: Handle the case that `target` can't even be instantiated. The error should say something like "more
          //       context needed" OR we could attempt to type the multi-function as its root type. This only works if
          //       there is only one root function, of course.
          instantiate(assignments, target, _.candidateType) match {
            case expectedFunctionType: FunctionType =>
              // TODO: Handle upper and lower bounds separately.
              resolveDispatch(mf, expectedFunctionType.inputTuple, position, assignments).flatMap { instance =>
                val actualFunctionType = instance.signature.functionType
                if (actualFunctionType.output <= expectedFunctionType.output) {
                  narrowBounds(assignments, target, actualFunctionType, actualFunctionType, judgment)
                } else {
                  Compilation.fail(MultiFunctionCoercion.IllegalOutput(mf, expectedFunctionType, actualFunctionType, position))
                }
              }

            case candidateType => Compilation.fail(MultiFunctionCoercion.FunctionContextExpected(mf, candidateType, position))
          }

        case ResolutionDirection.Backwards => illegalBackwards
      }

      case TypingJudgment.MostSpecific(reference, alternatives, position) => ??? // TODO: Implement.
      case TypingJudgment.Conjunction(judgments, position) => ??? // TODO: Implement.
    }
  }

  /**
    * TODO: Handle both upper and lower bounds separately.
    */
  private def resolveDispatch(mf: MultiFunctionDefinition, uninstantiatedInputType: ProductType, position: Position, assignments: Inference.Assignments): Compilation[FunctionInstance] = {
    val inputType = instantiate(assignments, uninstantiatedInputType, _.candidateType).asInstanceOf[ProductType]
    mf.min(inputType) match {
      case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType)(position))
      case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min)(position))
      case functionDefinition +: _ => functionDefinition.instantiate(inputType)
    }
  }

}
