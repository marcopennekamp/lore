package lore.compiler.phases.typing.inference

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Position}
import lore.compiler.phases.transformation.ExpressionBuilder.{AmbiguousCall, EmptyFit}
import lore.compiler.phases.transformation.ExpressionTransformationVisitor.CollectionExpected
import lore.compiler.phases.typing.inference.Inference.{Assignments, instantiate, instantiateByBound}
import lore.compiler.phases.typing.inference.InferenceBounds.{BoundType, ensureBound, narrowBounds}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.members.Member
import lore.compiler.types.HasMembers.MemberNotFound
import lore.compiler.types.{LeastUpperBound, ListType, MapType, ProductType}

object JudgmentResolver {

  sealed trait ResolutionDirection
  object ResolutionDirection {
    case object Forwards extends ResolutionDirection
    case object Backwards extends ResolutionDirection
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
          implicit val callPosition: Position = position
          // TODO: Candidate type or separate lower and upper bounds?
          val inputType = ProductType(arguments.map(tpe => instantiate(assignments, tpe, _.candidateType)))
          mf.min(inputType) match {
            case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType))
            case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min))
            case functionDefinition +: _ =>
              functionDefinition.instantiate(inputType).flatMap { instance =>
                val result = instance.signature.outputType
                narrowBounds(assignments, target, result, result, judgment)
              }
          }

        case ResolutionDirection.Backwards => illegalBackwards
      }

      case TypingJudgment.MostSpecific(reference, alternatives, position) => ??? // TODO: Implement.
      case TypingJudgment.Conjunction(judgments, position) => ??? // TODO: Implement.
    }
  }

}
