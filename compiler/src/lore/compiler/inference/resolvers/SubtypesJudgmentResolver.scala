package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.inference.InferenceBounds.{BoundType, ensureBounds, ensureLowerBound, ensureUpperBound}
import lore.compiler.inference.TypingJudgment
import lore.compiler.inference.matchers.SubtypingMatcher
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object SubtypesJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Subtypes] {

  override def nondirectional(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    ensureSubtypes(judgment.t1, judgment.t2, assignments, judgment)
  }

  private val ensureSubtypes: (Type, Type, Assignments, TypingJudgment) => Compilation[Assignments] = SubtypingMatcher.matchSubtype(
    (iv1, t2, assignments, context) =>
      ensureUpperBound(assignments, iv1, instantiateByBound(assignments, t2, BoundType.Upper), context).flatMap {
        assignments2 => ensureSubtypes(instantiateByBound(assignments2, iv1, BoundType.Lower), t2, assignments2, context)
      },
    (t1, iv2, assignments, context) =>
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, t1, BoundType.Lower), context).flatMap {
        assignments2 => ensureSubtypes(t1, instantiateByBound(assignments2, iv2, BoundType.Upper), assignments2, context)
      },
    (iv1, iv2, assignments, context) =>
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, iv1, BoundType.Lower), context).flatMap {
        assignments2 => ensureUpperBound(assignments2, iv1, instantiateByBound(assignments2, iv2, BoundType.Upper), context)
      }
  )

}
