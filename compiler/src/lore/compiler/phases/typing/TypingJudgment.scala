package lore.compiler.phases.typing

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.Type

sealed trait TypingJudgment extends Positioned {
  override def toString: String = TypingJudgment.stringify(this)
}

// TODO: Support custom error messages instead of (only) positions!
// TODO: Rename to TypingConstraint or TypeConstraint?

object TypingJudgment {

  /**
    * Models an operation with a fixed `target` inference variable and a list of `operands`.
    */
  sealed trait Operation extends TypingJudgment {
    def target: InferenceVariable

    /**
      * The list of operand types that need to be fully resolved before the operation can be resolved itself.
      */
    def operands: Vector[Type]
  }

  /**
    * Asserts that the given types must be equal to each other.
    */
  case class Equals(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Asserts that `t1` must be a subtype of `t2`.
    */
  case class Subtypes(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Assigns the least upper bound of the given types to the `target` inference variable.
    */
  case class LeastUpperBound(target: InferenceVariable, types: Vector[Type], position: Position) extends Operation {
    override def operands: Vector[Type] = types
  }

  /**
    * Simulates access of a member called `name` on the `source` type and assigns the member's result type to `target`.
    */
  case class MemberAccess(target: InferenceVariable, source: Type, name: String, position: Position) extends Operation {
    override def operands: Vector[Type] = Vector(source)
  }

  /**
    * Assigns the element type of `collection` to `target`.
    */
  case class ElementType(target: InferenceVariable, collection: Type, position: Position) extends Operation {
    override def operands: Vector[Type] = Vector(collection)
  }

  /**
    * Simulates a multi-function call given the multi-function `mf` and the argument types `arguments`. The call's
    * return type is assigned to `target`.
    */
  case class MultiFunctionCall(target: InferenceVariable, mf: MultiFunctionDefinition, arguments: Vector[Type], position: Position) extends Operation {
    override def operands: Vector[Type] = arguments
  }

  /**
    * From the given list of `alternatives`, the system first chooses all alternatives that result in a correct
    * inference result. Each alternative must result in a correct typing of the given `reference`. Finally, the
    * alternative which produces the most specific `reference` type wins and gets chosen. If multiple reference types
    * are equally specific, the typing judgment fails and results in an appropriate error.
    *
    * This typing judgment might seem esoteric at first glance, but it can be used to simulate argument selection of
    * multiple dispatch. Choosing the most specific `reference` type corresponds to choosing the most specific input
    * type in multiple dispatch, i.e. the `min` construction.
    *
    * TODO: We should be able to pass custom "no results" (corresponding to empty fit) and "too many results"
    *       (corresponding to ambiguous call) errors.
    */
  case class MostSpecific(reference: InferenceVariable, alternatives: Vector[TypingJudgment], position: Position) extends TypingJudgment

  case class Conjunction(judgments: Vector[TypingJudgment], position: Position) extends TypingJudgment

  def stringify(judgment: TypingJudgment): String = judgment match {
    case Equals(t1, t2, _) => s"$t1 :=: $t2"
    case Subtypes(t1, t2, _) => s"$t1 :<: $t2"
    case LeastUpperBound(target, types, _) => s"$target <- lub(${types.mkString(", ")})"
    case MemberAccess(target, source, name, _) => s"$target <- $source.$name"
    case ElementType(target, collection, _) => s"$target <- $collection::elementType"
    case MultiFunctionCall(target, mf, arguments, _) => s"$target <- ${mf.name}(${arguments.mkString(", ")})"
    case MostSpecific(reference, alternatives, _) =>s"$reference <- mostSpecific(\n    ${alternatives.mkString(",\n    ")}\n)"
    case Conjunction(judgments, _) => judgments.mkString(" & ")
  }

}
