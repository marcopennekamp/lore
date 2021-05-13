package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.InferenceBounds.BoundType
import lore.compiler.phases.transformation.inference.{TypeMatcher, TypingJudgment}
import lore.compiler.semantics.Registry

object SubtypesJudgmentResolver extends JudgmentResolver[TypingJudgment.Subtypes] {

  override def forwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    TypeMatcher.ensureBounds(assignments, judgment.t2, judgment.t1, BoundType.Upper, judgment)
  }

  override def backwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    TypeMatcher.ensureBounds(assignments, judgment.t1, judgment.t2, BoundType.Lower, judgment)
  }

}
