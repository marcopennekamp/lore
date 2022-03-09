package lore.compiler.typing.synthesizer

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.StructConstructorBinding
import lore.compiler.types.{Type, TypeVariable}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.synthesizer.ParametricFunctionSynthesizer.KnownArgumentTypes

object ConstructorCallSynthesizer {

  /**
    * Infers the constructor's type parameter assignments given a constructor call.
    *
    * Note that in the case of `binding` being a struct alias, we always infer the type arguments for the alias, not
    * for the underlying struct schema. The instantiated struct type's bounds are ensured by the definition of the
    * struct alias, which must define already compatible bounds for its own type parameters.
    */
  def infer(
    binding: StructConstructorBinding,
    expression: Expression.Call,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    Typing.logger.trace(s"Inference of constructor call `${expression.position.truncatedCode}`:")
    Typing.indentationLogger.indented {
      val (knownArgumentTypes, assignments2) = ParametricFunctionSynthesizer.preprocessArguments(expression.arguments, assignments)
      val (typeParameterAssignments, parameterTypes) = ParametricFunctionSynthesizer.prepareParameterTypes(binding.signature)
      inferAndAssign(binding, expression, typeParameterAssignments, parameterTypes, knownArgumentTypes, assignments2)
    }
  }

  /**
    * This function represents the last step of constructor call checking <b>and</b> inference.
    */
  def inferAndAssign(
    binding: StructConstructorBinding,
    expression: Expression.Call,
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    parameterTypes: Vector[Type],
    knownArgumentTypes: KnownArgumentTypes,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    ParametricFunctionSynthesizer
      .inferArgumentTypes(binding.typeParameters, typeParameterAssignments, parameterTypes, expression.arguments, knownArgumentTypes, assignments)
      .flatMap { argumentCandidate =>
        val resultType = binding.instantiateStructType(argumentCandidate.typeParameterAssignments)
        InferenceVariable.assign(expression.tpe.asInstanceOf[InferenceVariable], resultType, argumentCandidate.assignments)
      }
  }

}
