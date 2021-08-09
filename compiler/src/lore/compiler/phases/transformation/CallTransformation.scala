package lore.compiler.phases.transformation

import lore.compiler.core.Position
import lore.compiler.feedback.DispatchFeedback.{AmbiguousCall, EmptyFit}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.inference.Inference.isFullyInstantiated
import lore.compiler.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types.{FunctionType, TupleType, Type}

object CallTransformation {

  /**
    * Builds a function value call from the given target and arguments.
    */
  def valueCall(target: Expression, arguments: Vector[Expression], position: Position)(implicit judgmentCollector: JudgmentCollector): Expression.Call = {
    // TODO (schemas): Split the argument typing judgments. This will allow the inference algorithm to resolve argument
    //                 typings on a case-by-case basis. In the case where `inputType` is an inference variable, we can
    //                 simply turn it into a tuple of inference variables (we know the number of arguments statically!).
    //                 This might also allow us to type a FunctionType's input type as a TupleType.

    // A call target must be a value with a function type.
    val (inputType, outputType) = target.tpe match {
      // If the target's type is defined now, we can take a shortcut, because it's definitely a function.
      case FunctionType(input, output) => (input, output)

      // May or may not be a function type, so we have to make sure that the type is even a function. The Assign
      // judgment ensures that the target's type is even a function type. For now, we don't want to infer the type of
      // the target based on the provided arguments, so we're relying on one-way type inference.
      case _ =>
        val inputType = new InferenceVariable
        val outputType = new InferenceVariable

        judgmentCollector.add(TypingJudgment.Assign(FunctionType(inputType, outputType), target.tpe, target.position))

        (inputType, outputType)
    }

    val argumentTypes = TupleType(arguments.map(_.tpe))

    // If the input type of the function isn't fully instantiated, the arguments should determine its inference
    // variables (especially type parameters) conclusively with a Fits judgment, which assigns the inference variables'
    // upper and lower bounds.
    // TODO (schemas): I don't like that we're using this solution to essentially get around the problem of having to
    //                 resolve a subtyping judgment twice. Perhaps we should actually split all subtyping judgments
    //                 into two parts, so that inference is guaranteed to process both directions. We will also have to
    //                 make sure that the "both bound assignments" portion of the Fits judgment is carried over.
    //                 Perhaps we just need a Fits judgment in this direction and a Subtypes judgment exclusively in
    //                 the other.
    if (!isFullyInstantiated(inputType)) {
      judgmentCollector.add(TypingJudgment.Fits(argumentTypes, inputType, target.position))
    }

    judgmentCollector.add(TypingJudgment.Subtypes(argumentTypes, inputType, target.position))

    Expression.Call(CallTarget.Value(target), arguments, outputType, position)
  }

  /**
    * Builds a simple multi-function call without inference. Argument types may not contain inference variables. Also
    * verifies that the multi-function call's output type is a subtype of the expected output type.
    *
    * If a call expression cannot be created due to any errors, an [[Expression.Hole]] is returned instead with the
    * expected output type.
    */
  def multiFunctionCall(
    functionName: String,
    arguments: Vector[Expression],
    expectedOutputType: Type,
    position: Position,
  )(implicit registry: Registry, reporter: Reporter): Expression = {
    val option = for {
      mf <- registry.resolveMultiFunction(functionName, position)
      inputType = TupleType(arguments.map(_.tpe))
      instance <- mf.dispatch(inputType, EmptyFit(mf, inputType, position), min => AmbiguousCall(mf, inputType, min, position))
      expression = Expression.Call(CallTarget.MultiFunction(mf), arguments, instance.signature.outputType, position)
      _ = verifyOutputType(expectedOutputType)(expression)
    } yield expression

    option.getOrElse(Expression.Hole(expectedOutputType, position))
  }

  case class IllegalReturnType(call: Expression.Call, expectedTypes: Vector[Type]) extends Feedback.Error(call) {
    override def message = s"Calling ${call.target} returns the illegal type ${call.tpe}.$expected"
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types: ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  /**
    * Verifies that the given call expression has the expected output type.
    */
  private def verifyOutputType(expectedType: Type)(call: Expression.Call)(implicit reporter: Reporter): Unit = {
    if (call.tpe </= expectedType) {
      reporter.error(IllegalReturnType(call, Vector(expectedType)))
    }
  }

}
