package lore.compiler.inference

import lore.compiler.core.Compilation
import lore.compiler.feedback.Feedback
import lore.compiler.inference.Inference.Assignments
import lore.compiler.types._
import org.scalatest.Assertion

trait InferenceSpec extends TypeSpec {

  def assertInferenceSuccess(assignments: (InferenceVariable, InferenceBounds)*)(result: Compilation[Assignments]): Assertion = {
    result shouldEqual Compilation.succeed(assignments.toMap)
  }

  def assertInferenceFailure(error: Feedback.Error)(result: Compilation[Assignments]): Assertion = {
    result shouldEqual Compilation.fail(error)
  }

  def assertInferenceFailureDisregardingErrors(result: Compilation[Assignments]): Assertion = {
    result should matchPattern { case _: Compilation.Failure[_] => }
  }

  object Assignment {
    def lower(iv: InferenceVariable, lowerBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, lowerBound, any)
    def upper(iv: InferenceVariable, upperBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, nothing, upperBound)
    def fixed(iv: InferenceVariable, bound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, bound, bound)
    def assignment(iv: InferenceVariable, lowerBound: Type, upperBound: Type): (InferenceVariable, InferenceBounds) = iv -> InferenceBounds(iv, lowerBound, upperBound)
  }

}
