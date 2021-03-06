package lore.compiler.inference.resolvers

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.InferenceOrder.InfluenceGraph
import lore.compiler.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types.ShapeType

object MemberAccessJudgmentResolver extends JudgmentResolver[TypingJudgment.MemberAccess] {

  override def forwards(
    judgment: TypingJudgment.MemberAccess,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    instantiateCandidateType(assignments, judgment.source).member(judgment.name, judgment.position).flatMap {
      member => narrowBounds(assignments, judgment.target, member.tpe, judgment)
    }
  }

  override def backwards(
    judgment: TypingJudgment.MemberAccess,
    assignments: Assignments,
    influenceGraph: InfluenceGraph,
    remainingJudgments: Vector[TypingJudgment],
  )(implicit registry: Registry, reporter: Reporter): Option[JudgmentResolver.Result] = {
    val TypingJudgment.MemberAccess(target, source, name, position) = judgment
    val subtypesJudgment = TypingJudgment.Subtypes(source, ShapeType(name -> target), position)

    Inference.logger.trace(s"Added typing judgment: $subtypesJudgment")

    Some((assignments, remainingJudgments :+ subtypesJudgment))
  }

}
