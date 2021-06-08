package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.transformation.inference.InferenceBounds.narrowBounds
import lore.compiler.phases.transformation.inference.TypingJudgment
import lore.compiler.semantics.Registry

object MemberAccessJudgmentResolver extends JudgmentResolver[TypingJudgment.MemberAccess] {

  override def forwards(
    judgment: TypingJudgment.MemberAccess,
    assignments: Assignments
  )(implicit registry: Registry): Compilation[Assignments] = {
    instantiate(assignments, judgment.source, _.candidateType).member(judgment.name)(judgment.position).flatMap {
      member => narrowBounds(assignments, judgment.target, member.tpe, judgment)
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

}
