package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.types.Type
import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.phases.transformation.inference.InferenceBounds.{BoundType, ensureBound}
import lore.compiler.phases.transformation.inference.{Inference, InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.members.Member
import lore.compiler.types.HasMembers.MemberNotFound

object MemberAccessJudgmentResolver extends JudgmentResolver[TypingJudgment.MemberAccess] {

  override def forwards(
    judgment: TypingJudgment.MemberAccess,
    assignments: Assignments
  )(implicit registry: Registry): Compilation[Assignments] = {
    // We definitely have to resolve the member here (as all inference variables in source are fully inferred), but
    // the condition that the member is present is slightly softer than one would expect. If EITHER the lower or
    // upper instance contains the member, that is enough. We know that the member exists within the source type in
    // general and can bound the target variable appropriately.
    memberAt(BoundType.Lower, judgment, assignments).flatMap { lowerMember =>
      memberAt(BoundType.Upper, judgment, assignments).flatMap { upperMember =>
        if (lowerMember.nonEmpty || upperMember.nonEmpty) {
          val compilationLower = lowerMember match {
            case Some(member) => ensureBound(assignments, judgment.target, member.tpe, BoundType.Lower, judgment)
            case None => Compilation.succeed(assignments)
          }

          compilationLower.flatMap { assignments2 =>
            upperMember match {
              case Some(member) => ensureBound(assignments2, judgment.target, member.tpe, BoundType.Upper, judgment)
              case None => Compilation.succeed(assignments2)
            }
          }
        } else {
          Compilation.fail(MemberNotFound(judgment.name, Inference.instantiate(assignments, judgment.source, _.candidateType), judgment.position))
        }
      }
    }
  }

  override def backwards(
    judgment: TypingJudgment.MemberAccess,
    assignments: Assignments
  )(implicit registry: Registry): Compilation[Assignments] = {
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

  private def memberAt(boundType: BoundType, judgment: TypingJudgment.MemberAccess, assignments: Assignments): Compilation[Option[Member]] = {
    if (InferenceVariable.isDefinedAt(assignments, judgment.source, boundType)) {
      instantiateByBound(assignments, judgment.source, boundType).member(judgment.name)(judgment.position).map(Some(_))
    } else None.compiled
  }

}
