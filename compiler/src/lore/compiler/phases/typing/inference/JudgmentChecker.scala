package lore.compiler.phases.typing.inference

import lore.compiler.core.{Compilation, Error}
import lore.compiler.core.Compilation.Verification
import lore.compiler.phases.typing.inference.Inference.{Assignments, instantiate}
import lore.compiler.types.Type

object JudgmentChecker {

  case class UnsatisfiedEquals(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :=: $t2 remains unsatisfied after type inference!"
  }

  case class UnsatisfiedSubtypes(judgment: TypingJudgment, t1: Type, t2: Type) extends Error(judgment) {
    override def message: String = s"The judgment $t1 :<: $t2 remains unsatisfied after type inference!"
  }

  /**
    * Checks the given typing judgment with instantiated types. This ensures that type equality and subtyping
    * relationships actually hold with the inferred type assignments.
    */
  def check(judgment: TypingJudgment, assignments: Assignments): Verification = judgment match {
    case TypingJudgment.Equals(t1, t2, _) =>
      val it1 = instantiate(assignments, t1, _.candidateType)
      val it2 = instantiate(assignments, t2, _.candidateType)
      if (it1 != it2) {
        Compilation.fail(UnsatisfiedEquals(judgment, it1, it2))
      } else Verification.succeed

    case TypingJudgment.Subtypes(t1, t2, _) =>
      val it1 = instantiate(assignments, t1, _.candidateType)
      val it2 = instantiate(assignments, t2, _.candidateType)
      if (!(it1 <= it2)) {
        Compilation.fail(UnsatisfiedSubtypes(judgment, it1, it2))
      } else Verification.succeed

    // TODO: Do we need to check other judgments?
    case _ => Verification.succeed
  }

}
