package lore.compiler.inference.resolvers

import lore.compiler.core.Position
import lore.compiler.feedback.DispatchFeedback.{AmbiguousCall, EmptyFit}
import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.{Assignments, instantiateCandidateType}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.{Inference, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionInstance, MultiFunctionDefinition}
import lore.compiler.types.TupleType

object MultiFunctionCallJudgmentResolver extends JudgmentResolver[TypingJudgment.MultiFunctionCall] {

  // TODO: We might also want to implement a mechanism that connects the output type of the multi-function call to the
  //       arguments. This can either happen with a sort of backwards call resolution (though we'd need the forward
  //       resolution at the same time), or in the multi-function hint, perhaps. (I could also make the case for
  //       merging the multi-function call judgment with the multi-function hint judgment, but the existence of
  //       multi-function value judgments complicates this.)
  //       In any case, here is the motivating example. Look at the `functions/filter-curried` example and then
  //       consider the following call:
  //          map(data, filterCurried(x => x == 1))
  //       The equality check doesn't type the `x` as anything, which is correct as any kind of values may be compared
  //       with the operator, so `x` is initially bounded as Any.
  //       However, we know that `data` has the type `[[Int]]`, so the anonymous function returned by `filterCurried`
  //       is invoked with an `[Int]` argument. Let's say `iv1` is the output type of the `filterCurried` call, which
  //       is simultaneously the type of the second argument of `map`. The multi-function hint of `map` will assign
  //       `iv1([Int] => Nothing, Any)` because of `B >: A` in the definition of `map` (`C` is not yet bounded). Given
  //       `iv1 <- filterCurried(iv2)` with `iv2 :<: iv3 => Boolean` and `iv1 :=: [iv3] => [iv3]` (`iv3` stands in for
  //       the A type variable), we should be able to infer `iv3` to be `Int`. (The Nothing on the right-hand side of
  //       `iv1` may make problems, though.) The typing judgment `iv1 :=: [iv3]` would be generated due to the fact
  //       that `iv1` is the output type of the call, namely `[A] => [A]` or `[iv3] => [iv3]` after substitution.
  //       If we can add this line of reasoning to the inference algorithm, we will be able to type lambdas through
  //       expected output types of multi-functions. This is only really relevant when the output type of a
  //       multi-function contains a type variable.
  //       If we cannot accomplish this, the example code above would have to be written as:
  //          map(data, filterCurried((x: Int) => x == 1))
  //       This would introduce significant bloat for any user trying to program in a functional style.

  override def forwards(
    judgment: TypingJudgment.MultiFunctionCall,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    resolveDispatch(judgment.mf, TupleType(judgment.arguments), judgment.position, assignments).flatMap { instance =>
      val result = instance.signature.outputType
      narrowBounds(assignments, judgment.target, result, judgment)
    }
  }

  def resolveDispatch(
    mf: MultiFunctionDefinition,
    uninstantiatedInputType: TupleType,
    position: Position,
    assignments: Inference.Assignments,
  )(implicit reporter: Reporter): Option[FunctionInstance] = {
    val inputType = instantiateCandidateType(assignments, uninstantiatedInputType).asInstanceOf[TupleType]
    mf.dispatch(inputType, EmptyFit(mf, inputType, position), min => AmbiguousCall(mf, inputType, min, position))
  }

}
