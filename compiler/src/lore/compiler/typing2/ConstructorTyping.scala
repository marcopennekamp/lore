package lore.compiler.typing2

import lore.compiler.core.Positioned
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedConstructorCall
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{BasicType, DeclaredType, Fit, TupleType, Type, TypeVariable}
import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.synthesizer.ArgumentSynthesizer.Result
import lore.compiler.typing2.unification.{InferenceAssignments, InferenceVariable2, Unification2}
import lore.compiler.utils.CollectionExtensions.VectorExtension

// TODO (multi-import): Tracing...

object ConstructorTyping {



  // TODO (multi-import): Document.
  def checkOrInferCall(
    expression: UntypedConstructorCall,
    expectedType: Option[DeclaredType],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {

    // TODO (multi-import): Try to do this without inference variables. If it doesn't work out, we can always fall back
    //                      to inference variables here.








    val binding = expression.target
    val structSchema = binding.underlyingSchema

    // TODO (multi-function): Expected type handling...


    withPreparedParameterTypes(binding.signature) { case (inferenceVariables, parameterTypes) =>

      // (1) Infer all arguments that can be inferred without type context.
      val (inferredArguments, context2) = expression.arguments.foldLeft((Vector.empty[Option[Expression]], context)) {
        case ((typedArguments, context2), argument) =>
          Synthesizer2.attempt(argument, context2)._1 match {
            case Some((typedArgument, context3)) => (typedArguments :+ Some(typedArgument), context3)
            case None => (typedArguments :+ None, context2)
          }
      }

      val (inferredArgumentTypes, inferrableParameterTypes) = inferredArguments
        .zip(parameterTypes)
        .flatMap {
          case (Some(expression), parameterType) => Some(expression.tpe, parameterType)
          case _ => None
        }
        .unzip



      // TODO (multi-import): No need for throwaway tuple types here. Though the "optimization" might not be worth the
      //                      hassle here. (The runtime does the same, but there "micro"-performance actually matters.)
      Fit.fitsAssignments(TupleType(inferredArgumentTypes), TupleType(inferrableParameterTypes)) match {
        case Some(inferredAssignments) =>

          ???

        case None =>
          // TODO (multi-import): Type parameters cannot be instantiated from the given arguments. Report an error.
          ???
          None
      }


    }

    // TODO (multi-import): This is from the argument synthesizer... Might also be relevant for other types of calls.


    // (2) Prepare parameter types by replacing type variables with inference variables.
    val (parameterTypeTuple, typeParameterAssignments) = InferenceVariable2.fromTypeVariables(
      TupleType(binding.signature.parameters.map(_.tpe)),
      binding.signature.typeParameters,
    )
    val parameterTypes = parameterTypeTuple.asInstanceOf[TupleType].elements

    // TODO (multi-import): Here begins the implementation akin to `ArgumentSynthesizer.inferArgumentTypes`.
    // (3) Assign types from the inferred arguments to the struct's type parameters.





    // val (knownArgumentTypes, assignments2) = ArgumentSynthesizer.preprocessArguments(expression.arguments, assignments)
    // val (typeParameterAssignments, parameterTypes) = ArgumentSynthesizer.prepareParameterTypes(binding.signature)
    // inferAndAssign(binding, expression, typeParameterAssignments, parameterTypes, knownArgumentTypes, assignments2)




    // val structSchema = binding.underlyingType.schema
    // val expectedStructType = expectedType match {
    //   case structType: StructType if structType.schema == structSchema => structType
    //   case _ => expectedType.specialize(structSchema).filterType[StructType].getOrElse {
    //     reporter.error(TypingFeedback.ConstructorCall.CannotSpecialize(binding, expectedType, expression))
    //     return None
    //   }
    // }
    //
    // val (typeParameterAssignments, parameterTypes) = ArgumentSynthesizer.prepareParameterTypes(binding.signature)
    //
    // // Given that we have an expected struct type, we must unify the expected constructor's parameters with the
    // // actual parameter types so that any inference variables already known can be preassigned. For example, let's
    // // say we have an expected struct type `Wrapper[Int, Any]` from the test case `language/inference/wrapper.lore`.
    // // That is, we know the input type of the wrapper's function property. The expected constructor parameter is thus
    // // typed as `Int => Any`. The unification takes care that the inference variable for `A` is assigned `Int` as an
    // // upper bound.
    // val expectedParameterTypes = expectedStructType.constructorSignature.parameters.map(_.tpe)
    // val assignments2 = Unification.unifySubtypes(parameterTypes, expectedParameterTypes, assignments).getOrElse {
    //   return None
    // }
    //
    // val (knownArgumentTypes, assignments3) = ArgumentSynthesizer.preprocessArguments(expression.arguments, assignments2)
    // ConstructorCallSynthesizer.inferAndAssign(binding, expression, typeParameterAssignments, parameterTypes, knownArgumentTypes, assignments3)





  }

  /**
    * This implementation of `checkArgumentTypes` doesn't yet instantiate a [[Result]] so that [[inferArgumentTypes]]
    * still has the inference variables of type parameter assignments in the resulting Assignments. This is because
    * [[instantiateResult]] removes inference variables in `typeParameterAssignments` from the resulting assignments.
    */
  private def checkArgumentTypesImpl(
    typeParameters: Vector[TypeVariable],
    parameterTypes: Vector[Type],
    argumentTypes: Vector[Type],
    assignments: InferenceAssignments,
    positioned: Positioned,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceAssignments] = {
    if (argumentTypes.length != parameterTypes.length) {
      reporter.error(TypingFeedback.Function.IllegalArity(argumentTypes.length, parameterTypes.length, positioned))
      return None
    }

    // This unification has two purposes:
    //   1. It checks that each argument type fits the parameter type.
    //   2. It assigns type arguments to type parameters.
    val assignments2 = Unification2.unifyFits(argumentTypes, parameterTypes, assignments).getOrElse {
      reporter.error(
        TypingFeedback.Function.IllegalArgumentTypes(
          argumentTypes,
          InferenceVariable2.instantiateCandidate(parameterTypes, assignments),
          positioned,
        )
      )
      return None
    }

    val assignments3 = if (typeParameters.nonEmpty) {
      Unification2.unifyTypeVariableBounds(typeParameters, typeParameterAssignments, assignments2).getOrElse {
        val typeArguments = InferenceVariable.instantiateCandidate(typeParameters.map(typeParameterAssignments), assignments2)
        reporter.error(TypingFeedback.Function.IllegalTypeArguments(typeArguments, typeParameters, context))
        return None
      }
    } else assignments2

    Some(assignments3)
  }

  def withPreparedParameterTypes(
    signature: FunctionSignature,
  )(f: (Vector[InferenceVariable2], Vector[Type]) => Option[InferenceAssignments]): Option[TypeVariable.Assignments] = {
    // (2) Prepare parameter types by replacing type variables with inference variables.
    val (parameterTypeTuple, tvToIv) = InferenceVariable2.fromTypeVariables(
      signature.inputType,
      signature.typeParameters,
    )
    val inferenceVariables = signature.typeParameters.map(tvToIv)
    val parameterTypes = parameterTypeTuple.asInstanceOf[TupleType].elements

    f(inferenceVariables, parameterTypes).map { inferenceAssignments =>
      tvToIv.map {
        case (tv, iv) => tv -> InferenceVariable2.instantiateCandidate(iv, inferenceAssignments)
      }
    }
  }




}
