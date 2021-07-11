package lore.compiler.inference.matchers

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.types.{TupleType, Type}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object Matchers {

  trait Processor {
    def processIv1(
      iv1: InferenceVariable,
      t2: Type,
      assignments: Assignments,
      context: TypingJudgment
    )(implicit reporter: Reporter): Option[Assignments] = throw new UnsupportedOperationException

    def processIv2(
      t1: Type,
      iv2: InferenceVariable,
      assignments: Assignments,
      context: TypingJudgment
    )(implicit reporter: Reporter): Option[Assignments] = throw new UnsupportedOperationException

    def processBoth(
      iv1: InferenceVariable,
      iv2: InferenceVariable,
      assignments: Assignments,
      context: TypingJudgment
    )(implicit reporter: Reporter): Option[Assignments] = throw new UnsupportedOperationException
  }

  def matchTuple(
    t1: TupleType,
    t2: TupleType,
    assignments: Assignments,
    rec: (Assignments, Type, Type) => Option[Assignments],
    fail: () => Option[Nothing],
  ): Option[Assignments] = {
    if (t1.elements.size == t2.elements.size) {
      t1.elements.zip(t2.elements).foldSome(assignments) {
        case (assignments2, (e1, e2)) => rec(assignments2, e1, e2)
      }
    } else fail()
  }

}
