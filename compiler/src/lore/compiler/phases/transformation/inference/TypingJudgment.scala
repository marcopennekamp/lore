package lore.compiler.phases.transformation.inference

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.Type

sealed trait TypingJudgment extends Positioned {
  override def toString: String = TypingJudgment.stringify(this)
}

// TODO: Support custom error messages instead of (only) positions!

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
    *
    * A Subtypes judgment `t1 :<: t2` can inform both the upper bound of t1 as well as the lower bound of t2:
    *   - In the upper-bound case, we instantiate t2's inference variables with their upper bounds. Consider the
    *     following example: `iv1 :<: iv2[Int, Real]`. Because iv2 can at most be `Real`, iv1's domain must also be
    *     restricted to be at most `Real`. If we gave iv1 the upper bound `Int`, we might later type iv2 as `Real` and
    *     iv1 would have an upper bound that's too narrow.
    *   - The lower-bound case is the dual to the upper-bound case. Given `iv2[Int, Real] :<: iv1`, we must assign to
    *     iv1 the lower bound `Int`. If we gave iv1 the lower bound `Real`, we might later type iv2 as `Int` and iv1
    *     would have a lower bound that's too narrow.
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
    * TODO: Rewrite this description with the change to graph-based type inference.
    *
    * A MemberAccess judgment `target <- source.name` is resolved as follows:
    *
    *   1. If `source`'s lower bound is defined and contains a member `name`, the judgment ensures that the lower bound
    *      of `target` is a supertype of the member's type.
    *   2. If `source`'s upper bound is defined and contains a member `name`, the judgment ensures that the upper bound
    *      of `target` is a subtype of the member's type.
    *   3. If `target`'s upper bound is defined, the judgment ensures that the upper bound of `source` is a subtype of
    *      the type `{ name: A }` with `A` being the upper-bound instantiation of `target`.
    *
    * During checking after principal inference, the following is ensured (given `target` and `source` instantiated
    * with their candidate types): `target == source.name`. That is, the member must exist and the inferred member type
    * must be equal to the actual member type. These checks are necessary because the resolution is not guaranteed to
    * infer the correct types.
    *
    * For example, checking will ensure that the upper bound of `target` is not restricted further than the `source`
    * would allow. Take these judgments (see test:inference:0001): `p :=: { m: C }`, `x <- p.m`, and `x :<: B` given
    * B < C. The upper bound of `x` will be narrowed to B, but checking requires its candidate type, in this case the
    * upper bound, to be C.
    *
    * One might expect this judgment to reject source types which don't contain the member; however, this would be
    * detrimental to smart type inference, since we want to be able to successively narrow the bounds of the source
    * type without rejecting the membership too early. During the course of type inference, `source` may well become a
    * type which contains the desired member. Failing too early, too greedily, would lead to some incorrect inference
    * edge cases. This is why (1) and (2) are only applied when the instantiated source contains the member.
    *
    * (1) and (2) only ensure the lower and upper bounds (instead of narrowing them). This is due to how more complex
    * member accesses evolve during fixed-point evaluation. For an example, consider test:inference:0002.
    *
    * Resolution (3) is used to infer the `source` type from the `target` member type (backwards inference). We cannot
    * infer the lower bound of `source` this way because shape types cannot subtype structs and shape types also aren't
    * good lower bounds when multiple properties are involved.
    *
    * TODO: Since we only check that the member actually exists during the checking phase, will that create problems
    *       for the backtracking we will need to add to resolve multi-function calls? A branch which would have been
    *       rejected MIGHT be valid because we are delaying membership checks, leading to incorrect inference of a
    *       multi-function call. (For example, an invalid call might suddenly be valid, or an otherwise unique call
    *       might become ambiguous.)
    *         - On the other hand, the target inference variable won't receive any type if the source type doesn't
    *           contain the member, so it would never be defined at all. So this might not be an issue at all, or only
    *           in a few narrow edge cases.
    */
  case class MemberAccess(target: InferenceVariable, source: Type, name: String, position: Position) extends TypingJudgment

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
    * Types the given multi-function as a function, `target` containing the resulting type. Candidate types are
    * narrowed down first by taking the context surrounding `target` into account.
    *
    * TODO: Add an example.
    */
  case class MultiFunctionValue(target: InferenceVariable, mf: MultiFunctionDefinition, position: Position) extends TypingJudgment

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
    *
    * TODO: Instead of modeling these complex judgments in FunctionTyping, it might actually be preferable to produce
    *       these ad-hoc typings when resolving the MultiFunctionCall judgment. We would then have to make that
    *       judgment resolvable in both directions.
    *       OR: Keep the MultiFunctionCall judgment as is and introduce a second "MultiFunctionHint" judgment that
    *       provides a "typing hint" to the arguments.
    */
  case class MostSpecific(reference: InferenceVariable, alternatives: Vector[TypingJudgment], position: Position) extends TypingJudgment

  case class Conjunction(judgments: Vector[TypingJudgment], position: Position) extends TypingJudgment

  // TODO: Rename to "most specific" or something? At least work this into the name somehow?
  case class MultiFunctionHint(mf: MultiFunctionDefinition, arguments: Vector[Type], position: Position) extends TypingJudgment {
    /**
      * This inference variable is used by [[InferenceOrder]] to set the MultiFunctionHint as a dependency of the
      * hint's arguments.
      */
    lazy val dependencyVariable: InferenceVariable = new InferenceVariable
  }

  def stringify(judgment: TypingJudgment): String = judgment match {
    case Equals(t1, t2, _) => s"$t1 :=: $t2"
    case Subtypes(t1, t2, _) => s"$t1 :<: $t2"
    case Assign(target, source, _) => s"$target <- $source"
    case LeastUpperBound(target, types, _) => s"$target :=: LUB(${types.mkString(", ")})"
    case MemberAccess(target, source, name, _) => s"$target <- $source.$name"
    case ElementType(target, collection, _) => s"$target <- $collection::elementType"
    case MultiFunctionCall(target, mf, arguments, _) => s"$target <- ${mf.name}(${arguments.mkString(", ")})"
    case MultiFunctionValue(function, mf, _) => s"$function <- ${mf.name} as function"
    case MostSpecific(reference, alternatives, _) =>s"$reference <- mostSpecific(\n    ${alternatives.mkString(";\n    ")}\n)"
    case Conjunction(judgments, _) => judgments.mkString(", ")
    case hint@MultiFunctionHint(mf, arguments, _) => s"${mf.name}::hint(${arguments.mkString(", ")}) <dependency ${hint.dependencyVariable}>"
  }

}
