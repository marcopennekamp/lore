package lore.compiler.typing.synthesizer

/*

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.types.BasicType
import lore.compiler.typing.InferenceVariable
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.unification.Unification
import lore.compiler.utils.CollectionExtensions.VectorExtension

object ArithmeticSynthesizer {

  /**
    * Checks the given operands using [[checkOperand]] and assigns a result type to the arithmetic operation by
    * considering the inferred operand types. If all operands result in an Int, the result type of `operation` is also
    * Int, and otherwise Real.
    */
  def infer(
    operation: Expression,
    operands: Vector[Expression],
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    operands
      .foldSome(assignments) { case (assignments2, operand) => checkOperand(operand, assignments2) }
      .flatMap { assignments =>
        val operandTypes = operands.map(InferenceVariable.instantiateCandidate(_, assignments))
        val resultType = if (operandTypes.forall(_ == BasicType.Int)) BasicType.Int else BasicType.Real
        Unification.unifyEquals(operation.tpe, resultType, assignments)
      }
  }

  /**
    * Checks that `operand` is an Int or Real. The Int check is attempted first to get the narrowest possible type.
    */
  def checkOperand(
    operand: Expression,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    checker.attempt(operand, BasicType.Int, assignments)._1.orElse {
      checker.check(operand, BasicType.Real, assignments)
    }
  }

}

*/
