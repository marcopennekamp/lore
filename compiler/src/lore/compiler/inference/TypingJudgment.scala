package lore.compiler.inference

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.Type

sealed trait TypingJudgment extends Positioned {
  /**
    * The source/target dependencies induced by the current typing judgment.
    */
  lazy val dependencies: Set[InferenceDependency] = InferenceDependency.dependenciesOf(this)

  override def toString: String = TypingJudgment.stringify(this)
}

object TypingJudgment {

  /**
    * Asserts that the given types must be equal to each other.
    */
  case class Equals(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Asserts that `t1` must be a subtype of `t2`.
    *
    * A Subtypes judgment `t1 :<: t2` can inform both the upper bound of t1 as well as the lower bound of t2:
    *
    *   - In the upper-bound case, we instantiate t2's inference variables with their upper bounds. Consider the
    *     following example: `iv1 :<: iv2` with `iv2(Int, Real)`. Because iv2 can at most be `Real`, iv1's domain must
    *     also be restricted to be at most `Real`. If we gave iv1 the upper bound `Int`, we might later type iv2 as
    *     `Real` and iv1 would have an upper bound that's too narrow.
    *   - The lower-bound case is the dual to the upper-bound case. Given `iv2 :<: iv1` with `iv2(Int, Real)`, we must
    *     assign to iv1 the lower bound `Int`. If we gave iv1 the lower bound `Real`, we might later type iv2 as `Int`
    *     and iv1 would have a lower bound that's too narrow.
    */
  case class Subtypes(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Assigns matching types in `source` (inference variables instantiated as candidate types) to inference variables
    * in `target`.
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
    * Asserts that `t1` must fit into `t2`, with inference variables in `t2` representing type variables.
    *
    * The judgment is unidirectional (t1 --> t2) and will only be processed when `t1` can be instantiated. Because
    * inference variables in `t2` represent type variables, any types from `t1` matching with an inference variable
    * `iv2` will be <b>assigned</b> to `iv2` in both bounds. This is consistent with how type variable allocations are
    * built.
    */
  case class Fits(t1: Type, t2: Type, position: Position) extends TypingJudgment

  /**
    * Ensures that `target` is the least upper bound of all `types`. The inference can happen in both directions:
    *
    *   1. If all inference variables in `types` are fully inferred, the least upper bounds of the instantiated types
    *      (both lower bound and upper bound instantiation) are assigned to the lower and upper bound of `target`.
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
    * A MemberAccess judgment `target <- source.member` is resolved as follows:
    *
    *   1. If `source` is fully inferred, the judgment ensures that the candidate instantiation of `source` contains
    *      a member `member`. The type of `member` is assigned to `target`.
    *   2. If `target` is fully inferred, the judgment ensures that the upper bound of `source` is a subtype of the
    *      type `{ member: A }` with `A` being the upper-bound instantiation of `target`.
    *
    * The target type is taken from the candidate type of `source` in resolution (1) to produce stable assignments. The
    * member's type held in `target` must always be equal to `source`'s member type, so the bounds must be fixed right
    * away. (The bounds of `source` are already unchanging, because it's fully inferred.) For an example showing why
    * this is important, see test:inference:member:0001.
    *
    * Resolution (2) is used to infer the `source` type from the `target` member type (backwards inference). We cannot
    * infer the lower bound of `source` this way because shape types cannot subtype structs and shape types also aren't
    * good lower bounds when multiple properties are involved.
    */
  case class MemberAccess(target: InferenceVariable, source: Type, name: String, position: Position) extends TypingJudgment

  /**
    * Assigns the element type of `collection` to `target`.
    */
  case class ElementType(target: InferenceVariable, collection: Type, position: Position) extends TypingJudgment

  /**
    * Simulates a multi-function call given the multi-function `mf` and the argument types `arguments`. The call's
    * return type is assigned to `target`.
    */
  case class MultiFunctionCall(target: InferenceVariable, mf: MultiFunctionDefinition, arguments: Vector[Type], position: Position) extends TypingJudgment

  /**
    * Types the given multi-function as a function, `target` containing the resulting type. Candidate types are
    * narrowed down first by taking the context surrounding `target` into account.
    */
  case class MultiFunctionValue(target: InferenceVariable, mf: MultiFunctionDefinition, position: Position) extends TypingJudgment

  /**
    * A MultiFunctionHint provides typing information to multi-function arguments. Usually, all arguments will be fully
    * typed, in which case the MultiFunctionHint serves no purpose and will be quickly skipped.
    *
    * In some cases, especially when anonymous function or multi-function value arguments are involved, the argument
    * needs the context information from the multi-function to be correctly typed. The judgment resolver computes a
    * list of possible function definitions and then runs argument type inference for each of them, choosing the most
    * specific argument type as the path to follow. The MultiFunctionHint resolver might run into an empty fit or
    * ambiguity error, which are reported as a compilation errors to the user.
    */
  case class MultiFunctionHint(mf: MultiFunctionDefinition, arguments: Vector[Expression], position: Position) extends TypingJudgment {
    val argumentTypes: Vector[Type] = arguments.map(_.tpe)

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
    case Fits(t1, t2, _) => s"$t1 fits $t2"
    case LeastUpperBound(target, types, _) => s"$target :=: LUB(${types.mkString(", ")})"
    case MemberAccess(target, source, name, _) => s"$target <- $source.$name"
    case ElementType(target, collection, _) => s"$target <- $collection::elementType"
    case MultiFunctionCall(target, mf, arguments, _) => s"$target <- ${mf.name}(${arguments.mkString(", ")})"
    case MultiFunctionValue(function, mf, _) => s"$function <- ${mf.name} as function"
    case hint@MultiFunctionHint(mf, _, _) => s"${mf.name}::hint(${hint.argumentTypes.mkString(", ")}) <dependency ${hint.dependencyVariable}>"
  }

  /**
    * Collects all inference variables contained in the given judgment.
    */
  def variables(judgment: TypingJudgment): Set[InferenceVariable] = judgment match {
    case Equals(t1, t2, _) => Inference.variables(t1) ++ Inference.variables(t2)
    case Subtypes(t1, t2, _) => Inference.variables(t1) ++ Inference.variables(t2)
    case Assign(target, source, _) => Inference.variables(target) ++ Inference.variables(source)
    case Fits(t1, t2, _) => Inference.variables(t1) ++ Inference.variables(t2)
    case LeastUpperBound(target, types, _) => Set(target) ++ types.flatMap(Inference.variables)
    case MemberAccess(target, source, _, _) => Set(target) ++ Inference.variables(source)
    case ElementType(target, collection, _) => Set(target) ++ Inference.variables(collection)
    case MultiFunctionCall(target, _, arguments, _) => Set(target) ++ arguments.flatMap(Inference.variables)
    case MultiFunctionValue(target, _, _) => Set(target)
    case hint@MultiFunctionHint(_, arguments, _) => Set(hint.dependencyVariable) ++ arguments.map(_.tpe).flatMap(Inference.variables)
  }

}
