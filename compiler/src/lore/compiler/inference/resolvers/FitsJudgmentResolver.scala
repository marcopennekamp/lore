package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
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
      // The first case is necessary for matching contravariant types such as function types.
      (iv1, t2, assignments, context) => narrowBounds(assignments, iv1, t2, context),
      (t1, iv2, assignments, context) => narrowBounds(assignments, iv2, t1, context),
      Matchers.unsupported,
    )(instantiateCandidateType(assignments, judgment.t1), judgment.t2, assignments, judgment)
  }

}
