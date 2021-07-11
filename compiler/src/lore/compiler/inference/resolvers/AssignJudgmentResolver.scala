package lore.compiler.inference.resolvers

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.matchers.{EqualityMatcher, Matchers}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object AssignJudgmentResolver extends JudgmentResolver[TypingJudgment.Assign] {

  override def forwards(
    judgment: TypingJudgment.Assign,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    EqualityMatcher.matchEquals(AssignProcessor)(instantiateCandidateType(assignments, judgment.source), judgment.target, assignments, judgment)
  }

  private object AssignProcessor extends Matchers.Processor {
    override def processIv2(t1: Type, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      narrowBounds(assignments, iv2, t1, context)
    }
  }

}
