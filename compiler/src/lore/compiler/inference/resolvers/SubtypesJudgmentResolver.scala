package lore.compiler.inference.resolvers

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, instantiateByBound}
import lore.compiler.inference.InferenceBounds.{BoundType, ensureLowerBound, ensureUpperBound}
import lore.compiler.inference.matchers.{Matchers, SubtypingMatcher}
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object SubtypesJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Subtypes] {

  override def nondirectional(
    judgment: TypingJudgment.Subtypes,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    ensureSubtypes(judgment.t1, judgment.t2, assignments, judgment)
  }

  private def ensureSubtypes(t1: Type, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
    SubtypingMatcher.matchSubtype(SubtypesProcessor)(t1, t2, assignments, context)
  }

  private object SubtypesProcessor extends Matchers.Processor {
    override def processIv1(iv1: InferenceVariable, t2: Type, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureUpperBound(assignments, iv1, instantiateByBound(assignments, t2, BoundType.Upper), context).flatMap {
        assignments2 => ensureSubtypes(instantiateByBound(assignments2, iv1, BoundType.Lower), t2, assignments2, context)
      }
    }

    override def processIv2(t1: Type, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, t1, BoundType.Lower), context).flatMap {
        assignments2 => ensureSubtypes(t1, instantiateByBound(assignments2, iv2, BoundType.Upper), assignments2, context)
      }
    }

    override def processBoth(iv1: InferenceVariable, iv2: InferenceVariable, assignments: Assignments, context: TypingJudgment)(implicit reporter: Reporter): Option[Assignments] = {
      ensureLowerBound(assignments, iv2, instantiateByBound(assignments, iv1, BoundType.Lower), context).flatMap {
        assignments2 => ensureUpperBound(assignments2, iv1, instantiateByBound(assignments2, iv2, BoundType.Upper), context)
      }
    }
  }

}
