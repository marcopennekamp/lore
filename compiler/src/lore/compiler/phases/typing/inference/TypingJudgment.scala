package lore.compiler.phases.typing.inference

import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.typing.inference.Inference.isFullyInferred
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
    * Assigns matching types in `source` to inference variables in `target`.
    *
    * This is used by variable declarations, for example, to ensure that the variable's type is fixed in one direction.
    * We don't want the following to be viable Lore code:
    *
    * let f = v => v.x > 5
    * f(%{ x: 10 })
    *
    * The type of the variable declaration should become fixed at the point of declaration.
    */
  case class Assign(target: Type, source: Type, position: Position) extends TypingJudgment

  /**
    * Unifies the `target` inference variable with the least upper bound of all given `types`. The inference can happen
    * in both directions:
    *
    *   1. If all inference variables in `types` are defined, the least upper bounds of the instantiated types (both
    *      lower bound and upper bound instantiation) are assigned to the lower and upper bound of `target`.
    *   2. If `target` is defined, the typing judgment `LUB(T, A_1, ..., A_n)` is effectively equal to
    *      `A_1 :<: T`, ..., `A_n :<: T`. These "virtual" judgments are resolved, allowing the upper bounds of all
    *      `A_x` to be defined through T.
    *
    * Crucially, this judgment is necessary because `LUB(T, A, B)` cannot be fully modeled as `Subtypes(A, T)` and
    * `Subtypes(B, T)`. The Subtypes judgments only specify the lower bound of T, but not the upper bound. Why we need
    * the upper bound specified might become clearer with an example: Assume we have an If-Else expression with body
    * types A and B. We assign `LUB(A, B)` to T as the result type of the If-Else expression. If we do not assign an
    * upper bound to T, a judgment such as `C :<: T` will have no choice but to infer Any as the upper bound of C. But
    * we already know that the value whose type is modeled by T can only be `a: A` or `b: B`. So there is an upper
    * bound for T, namely the LUB of A and B. C, then, also has the same upper bound.
    */
  case class LeastUpperBound(target: InferenceVariable, types: Vector[Type], position: Position) extends TypingJudgment

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

  def isSimple(judgment: TypingJudgment): Boolean = judgment match {
    case Equals(t1, t2, _) => isFullyInferred(t1) || isFullyInferred(t2)
    case Subtypes(t1, t2, _) => isFullyInferred(t1) || isFullyInferred(t2)
    case Assign(_, source, _) => isFullyInferred(source)
    case LeastUpperBound(_, types, _) => types.forall(isFullyInferred)
    case operation: Operation => operation.operands.forall(isFullyInferred)
    case MostSpecific(_, alternatives, _) => alternatives.forall(isSimple)
    case Conjunction(judgments, _) => judgments.forall(isSimple)
  }

  def stringify(judgment: TypingJudgment): String = judgment match {
    case Equals(t1, t2, _) => s"$t1 :=: $t2"
    case Subtypes(t1, t2, _) => s"$t1 :<: $t2"
    case Assign(target, source, _) => s"$target <- $source"
    case LeastUpperBound(target, types, _) => s"$target :=: LUB(${types.mkString(", ")})"
    case MemberAccess(target, source, name, _) => s"$target <- $source.$name"
    case ElementType(target, collection, _) => s"$target <- $collection::elementType"
    case MultiFunctionCall(target, mf, arguments, _) => s"$target <- ${mf.name}(${arguments.mkString(", ")})"
    case MostSpecific(reference, alternatives, _) =>s"$reference <- mostSpecific(\n    ${alternatives.mkString(",\n    ")}\n)"
    case Conjunction(judgments, _) => judgments.mkString(" & ")
  }

}
