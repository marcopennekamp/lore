package lore.compiler.inference.resolvers

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.matchers.{Matchers, SubtypingMatcher}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object FitsJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Fits] {

  override def nondirectional(
    judgment: TypingJudgment.Fits,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    SubtypingMatcher.matchSubtype(FitsProcessor)(instantiateCandidateType(assignments, judgment.t1), judgment.t2, assignments, judgment)
  }

  private object FitsProcessor extends Matchers.Processor {
    override def processIv1(iv1: InferenceVariable, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      // This case is necessary for matching contravariant types such as function types.
      narrowBounds(assignments, iv1, t2, context)
    }

    override def processIv2(t1: Type, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      narrowBounds(assignments, iv2, t1, context)
    }
  }

}
