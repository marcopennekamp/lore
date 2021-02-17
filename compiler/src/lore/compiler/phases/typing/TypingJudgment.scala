package lore.compiler.phases.typing

import lore.compiler.core.{Position, Positioned}
import lore.compiler.types.Type

sealed trait TypingJudgment extends Positioned

object TypingJudgment {

  /**
    * Asserts that the given types must be equal to each other.
    */
  case class Equals(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Asserts that `t1` must be a subtype of `t2`.
    */
  case class Subtypes(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Simulates access of a member called `name` on the `source` type and assigns the member's result type to `target`.
    */
  case class MemberAccess(target: InferenceVariable, source: Type, name: String, position: Position) extends TypingJudgment

  /**
    * Assigns the least upper bound of the given types to the `target` inference variable.
    */
  case class LeastUpperBound(target: InferenceVariable, types: Vector[Type], position: Position) extends TypingJudgment

  // TODO: Is this even feasible?
  case class Alternative(judgments: Vector[TypingJudgment], position: Position) extends TypingJudgment

}
