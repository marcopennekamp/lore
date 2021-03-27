package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.phases.transformation.inference.{InferenceVariable, TypingJudgment}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{CallTarget, FunctionSignature}
import lore.compiler.types.{BasicType, FunctionType, ProductType, Type}

object FunctionTyping {

  case class WrongNumberOfArguments(signature: FunctionSignature, callPos: Position) extends Error(callPos) {
    override def message: String = s"The function/constructor ${signature.name} was called with the wrong number of arguments." +
      s" Expected: ${signature.parameters.size}."
  }

  /**
    * Checks that the given arguments adhere to the given signature.
    *
    * We are assuming that the signature is fixed, so don't use this for multi-functions!
    */
  def adhereToSignature(arguments: Vector[Expression], signature: FunctionSignature, position: Position): Compilation[Vector[TypingJudgment]] = {
    val parameterTypes = signature.parameters.map(_.tpe)
    if (parameterTypes.size != arguments.size) {
      Compilation.fail(WrongNumberOfArguments(signature, position))
    } else {
      parameterTypes.zip(arguments).map { case (parameterType, argument) =>
        TypingJudgment.Subtypes(argument.tpe, parameterType, argument.position)
      }.compiled
    }
  }

  /**
    * Builds a simple multi-function call and the corresponding inference judgments needed to pick the multi-function.
    * Cannot be used to build constructor calls!
    *
    * TODO: Write a more comprehensive documentation.
    *
    * TODO: We somehow have to add additional type inference rules that allow us to type some expressions using the
    *       argument types of the multi-function. For example, let's take this map function:
    *           function map(as: [A], f: A => B): [B] where A, B = for (a <- as) f(a)
    *       We want to call it like this:
    *           map(
    *             [%{ name: 'Alpha' }, %{ name: 'Beta' }, %{ name: 'Gamma' }],
    *             thing => thing.name
    *           )
    *       The type inference algorithm should understand the following:
    *         1. The first argument's type is [{ name: String }], so we have to set A = { name: String }.
    *         2. A = { name: String }, so f's parameter type must be { name: String }.
    *         3. B = String, because { name: String }.name = String.
    *         4. The result type of the call is [String].
    *       This could be modeled by the following typing judgments:
    *         1. [v_A] :=: [{ name: String }]
    *            (We have to model the function's type variables as inference variables.)
    *         2. No judgments necessary if we properly model A as v_A.
    *         3. v_B = (thing => thing.name).outputType
    *         4. No judgments necessary if we properly model B as v_B.
    *       In general:
    *         - For each argument: arg.type :<: param.tpe ???
    *         - The result type must be instantiated from the correct inference variables.
    *         -
    *       To accomplish this, we MIGHT have to model the fit/min operation with general typing judgments.
    */
  def multiFunctionCall(
    functionName: String,
    arguments: Vector[Expression],
    position: Position
  )(implicit registry: Registry): Compilation[(Expression.Call, Vector[TypingJudgment])] = {
    implicit val callPosition: Position = position
    registry.resolveMultiFunction(functionName).map { mf =>
      // In essence, we want to choose the most specific argument type to then invoke multiple dispatch with the
      // "result argument type". Hence, this inference variable is the container that holds the ultimate argument type.
      val resultArgumentType = new InferenceVariable

      // We can filter by arity immediately, because a function with a different arity will never be callable with the
      // given arguments.
      val alternatives = mf.functions.filter(_.signature.arity == arguments.length).map { f =>
        val originalType = f.signature.functionType
        val variables = f.typeScope.localTypeVariables
        val (functionType, boundJudgments) = if (variables.nonEmpty) {
          // Replace all type variables declared in the function with inference variables. The inference algorithm
          // needs to do the actual work here. The "fast track" Fits/TypeVariableAllocation is not applicable with
          // typing judgments.
          val assignments = variables.map(variable => (variable, new InferenceVariable)).toMap

          // We have to model type variable bounds as typing judgments so that the bounds are taken into account during
          // inference.
          var boundJudgments = Vector.empty[TypingJudgment]
          variables.foreach { variable =>
            if (variable.upperBound != BasicType.Any) {
              boundJudgments = boundJudgments :+ TypingJudgment.Subtypes(assignments(variable), Type.substitute(assignments, variable.upperBound), position)
            }

            if (variable.lowerBound != BasicType.Nothing) {
              boundJudgments = boundJudgments :+ TypingJudgment.Subtypes(Type.substitute(assignments, variable.lowerBound), assignments(variable), position)
            }
          }

          (
            Type.substitute(assignments, originalType).asInstanceOf[FunctionType], // TODO: Can we resolve the manual cast?
            boundJudgments
          )
        } else (originalType, Vector.empty)

        // TODO: Possibly pre-filter by already known argument types.
        // TODO: These judgments are only necessary if the argument type contains external inference variables, right?
        // For each feasible function type, add an alternative typing for the arguments. This gives the inference
        // algorithm enough information to infer types of arguments should they not type themselves, which applies
        // to anonymous function expressions, for example.
        val argumentJudgments = functionType.input.elements.zip(arguments).map { case (parameterType, argument) =>
          TypingJudgment.Subtypes(argument.tpe, parameterType, argument.position)
        }

        // We also need to set the "result argument type" to something so that ultimately the most specific argument
        // type can be chosen.
        val resultJudgments = Vector(
          TypingJudgment.Equals(resultArgumentType, ProductType(arguments.map(_.tpe)), position)
        )

        TypingJudgment.Conjunction(boundJudgments ++ argumentJudgments ++ resultJudgments, position)
      }

      val resultType = new InferenceVariable
      val judgments = Vector(
        // TODO: Pass EmptyFit and AmbiguousCall errors to "most specific". They conceptually correspond one to one.
        //          case min if min.isEmpty => Compilation.fail(EmptyFit(mf, inputType))
        //          case min if min.size > 1 => Compilation.fail(AmbiguousCall(mf, inputType, min))
        // TODO: Activate this once we actually need to infer argument types via their usage contexts. This will only
        //       become truly relevant with anonymous functions.
        //TypingJudgment.MostSpecific(resultArgumentType, alternatives, position),

        // TODO: This will technically be a bit redundant, since we'd already have modeled the dispatch using typing judgments above.
        TypingJudgment.MultiFunctionCall(resultType, mf, arguments.map(_.tpe), position),
      )

      (
        Expression.Call(CallTarget.MultiFunction(mf, resultType), arguments, position),
        judgments
      )
    }
  }

}
