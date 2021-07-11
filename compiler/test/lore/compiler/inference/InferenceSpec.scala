package lore.compiler.inference

import lore.compiler.feedback.{Feedback, MemoReporter}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.types._
import org.scalatest.Assertion

trait InferenceSpec extends TypeSpec {

  type InferenceResult = (Assignments, Vector[Feedback])

  def infer(judgments: TypingJudgment*): InferenceResult = {
    val reporter: MemoReporter = MemoReporter()
    val assignments = Inference.infer(judgments.toVector)(null, reporter)
    (assignments, reporter.feedback)
  }

  def assertInferenceSuccess(expectedAssignments: (InferenceVariable, InferenceBounds)*)(result: InferenceResult): Assertion = {
    val (assignments, feedback) = result
    feedback.filter(_.isError) shouldBe empty
    assignments shouldEqual expectedAssignments.toMap
  }

  def assertInferenceFailure(error: Feedback.Error)(result: InferenceResult): Assertion = {
    val (_, feedback) = result
    feedback shouldEqual Vector(error)
  }

  def assertInferenceFailureDisregardingErrors(result: InferenceResult): Assertion = {
    val (_, feedback) = result
    feedback shouldNot be(empty)
  }

  object Assignment {
    def lower(iv: InferenceVariable, lowerBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, lowerBound, any)
    def upper(iv: InferenceVariable, upperBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, nothing, upperBound)
    def fixed(iv: InferenceVariable, bound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, bound, bound)
    def assignment(iv: InferenceVariable, lowerBound: Type, upperBound: Type): (InferenceVariable, InferenceBounds) = iv -> InferenceBounds(iv, lowerBound, upperBound)
  }

}
