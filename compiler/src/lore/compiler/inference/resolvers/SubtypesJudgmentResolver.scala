package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.inference.InferenceBounds.{BoundType, ensureBoundSubtypes, ensureBoundSupertypes}
import lore.compiler.inference.TypingJudgment
import lore.compiler.inference.matchers.{Matchers, SubtypingMatcher}
import lore.compiler.semantics.Registry

object SubtypesJudgmentResolver extends JudgmentResolver[TypingJudgment.Subtypes] {

  override def forwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    ensureSubtypes(judgment.t1, instantiateByBound(assignments, judgment.t2, BoundType.Upper), assignments, judgment)
  }

  override def backwards(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    ensureSubtypes(instantiateByBound(assignments, judgment.t1, BoundType.Lower), judgment.t2, assignments, judgment)
  }

  private val ensureSubtypes = SubtypingMatcher.matchSubtype(
    (iv1, t2, assignments, context) => ensureBoundSubtypes(assignments, iv1, t2, context),
    (t1, iv2, assignments, context) => ensureBoundSupertypes(assignments, iv2, t1, context),
    Matchers.unsupported,
  ) _

}