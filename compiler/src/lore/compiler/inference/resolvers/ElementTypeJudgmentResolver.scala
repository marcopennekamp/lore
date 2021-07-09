package lore.compiler.inference.resolvers

import lore.compiler.feedback.Reporter
import lore.compiler.feedback.TypingFeedback.CollectionExpected
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.Registry
import lore.compiler.types.{ListType, MapType, TupleType}

object ElementTypeJudgmentResolver extends JudgmentResolver[TypingJudgment.ElementType] {

  override def forwards(
    judgment: TypingJudgment.ElementType,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    val instantiatedCollection = instantiateCandidateType(assignments, judgment.collection)
    val elementType = instantiatedCollection match {
      case ListType(element) => Some(element)
      case MapType(key, value) => Some(TupleType(Vector(key, value)))
      case _ =>
        reporter.error(CollectionExpected(instantiatedCollection, judgment))
        None
    }
    elementType.flatMap(tpe => narrowBounds(assignments, judgment.target, tpe, judgment))
  }

}
