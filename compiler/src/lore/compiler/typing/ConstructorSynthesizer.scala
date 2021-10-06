package lore.compiler.typing

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.StructConstructorBinding
import lore.compiler.types.TypeVariableAllocation

object ConstructorSynthesizer {

  /**
    * Infers the constructor's type parameter assignments given a constructor call.
    */
  def infer(
    binding: StructConstructorBinding,
    expression: Expression.Call,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Assignments = {
    val (knownArgumentTypes, assignments2) = ParametricFunctionSynthesizer.preprocessArguments(expression.arguments, assignments)

    ParametricFunctionSynthesizer.inferArgumentType(binding.signature, expression.arguments, knownArgumentTypes, assignments2) match {
      case Some(argumentCandidate) =>
        val resultType = binding.asSchema.instantiate(argumentCandidate.typeVariableAssignments).constructorSignature.outputType
        Helpers.assign(expression.tpe.asInstanceOf[InferenceVariable], resultType, argumentCandidate.assignments)
          .getOrElse(argumentCandidate.assignments)

      case None => assignments2
    }
  }

}
