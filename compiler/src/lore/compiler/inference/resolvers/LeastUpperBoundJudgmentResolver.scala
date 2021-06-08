package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.inference.InferenceBounds.{BoundType, narrowBounds}
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.Registry
import lore.compiler.types.LeastUpperBound

object LeastUpperBoundJudgmentResolver extends JudgmentResolver[TypingJudgment.LeastUpperBound] {

  override def forwards(
    judgment: TypingJudgment.LeastUpperBound,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // The least upper bound calculated from the given types has to be calculated from the lower bounds and the
    // upper bounds separately.
    val lowerLub = LeastUpperBound.leastUpperBound(judgment.types.map(instantiateByBound(assignments, _, BoundType.Lower)))
    val upperLub = LeastUpperBound.leastUpperBound(judgment.types.map(instantiateByBound(assignments, _, BoundType.Upper)))
    narrowBounds(assignments, judgment.target, lowerLub, upperLub, judgment)
  }

  override def backwards(
    judgment: TypingJudgment.LeastUpperBound,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // TODO: Implement the target -> types (backwards) direction.
    ???
  }

}
