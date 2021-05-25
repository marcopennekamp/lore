package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.transformation.inference.InferenceBounds.narrowBounds
import lore.compiler.phases.transformation.inference.TypingJudgment
import lore.compiler.phases.transformation.inference.matchers.{EqualityMatcher, Matchers}
import lore.compiler.semantics.Registry

object AssignJudgmentResolver extends JudgmentResolver[TypingJudgment.Assign] {

  override def forwards(
    judgment: TypingJudgment.Assign,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    EqualityMatcher.matchEquals(
      Matchers.unsupported,
      (t1, iv2, assignments, context) => narrowBounds(assignments, iv2, t1, t1, context),
      Matchers.unsupported,
    )(instantiate(assignments, judgment.source, _.candidateType), judgment.target, assignments, judgment)
  }

}
