package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.{TypeMatcher, TypingJudgment}
import lore.compiler.semantics.Registry

object AssignJudgmentResolver extends JudgmentResolver[TypingJudgment.Assign] {

  override def forwards(
    judgment: TypingJudgment.Assign,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    TypeMatcher.narrowBounds(assignments, judgment.source, judgment.target, judgment)
  }

}
