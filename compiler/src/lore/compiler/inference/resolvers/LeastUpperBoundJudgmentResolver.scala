package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.inference.Inference.{Assignments, instantiateByBound, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.{BoundType, narrowBounds}
import lore.compiler.inference.InferenceOrder.InfluenceGraph
import lore.compiler.inference.{Inference, TypingJudgment}
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
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry): Compilation[JudgmentResolver.Result] = {
    val TypingJudgment.LeastUpperBound(target, types, position) = judgment
    val subtypesJudgments = types.map(t => TypingJudgment.Subtypes(t, instantiateCandidateType(assignments, target), position))

    Inference.logger.trace(s"Added typing judgments:\n${subtypesJudgments.mkString("\n")}")

    (assignments, remainingJudgments ++ subtypesJudgments).compiled
  }

}
