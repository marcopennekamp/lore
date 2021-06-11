package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.inference.matchers.{EqualityMatcher, Matchers}
import lore.compiler.semantics.Registry

object AssignJudgmentResolver extends JudgmentResolver[TypingJudgment.Assign] {

  override def forwards(
    judgment: TypingJudgment.Assign,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    EqualityMatcher.matchEquals(
      Matchers.unsupported,
      (t1, iv2, assignments, context) => narrowBounds(assignments, iv2, t1, context),
      Matchers.unsupported,
    )(instantiateCandidateType(assignments, judgment.source), judgment.target, assignments, judgment)
  }

}
