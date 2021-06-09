package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiate}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.inference.matchers.{Matchers, SubtypingMatcher}
import lore.compiler.semantics.Registry

object FitsJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Fits] {

  override def nondirectional(
    judgment: TypingJudgment.Fits,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    SubtypingMatcher.matchSubtype(
      Matchers.unsupported,
      (t1, iv2, assignments, context) => narrowBounds(assignments, iv2, t1, context),
      Matchers.unsupported,
    )(instantiate(assignments, judgment.t1, _.candidateType), judgment.t2, assignments, judgment)
  }

}