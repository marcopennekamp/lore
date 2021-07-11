package lore.compiler.inference

import lore.compiler.core.Position
import lore.compiler.types.ListType

/**
  * Ensures that typing judgments mirroring the typings of common language operations such as addition or appending are
  * correctly resolved.
  */
class OperationInferenceSpec extends InferenceSpec {

  "Inference" should "infer the result type of an addition" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = infer(
      TypingJudgment.Subtypes(a, real, Position.internal),
      TypingJudgment.Subtypes(b, real, Position.internal),
      TypingJudgment.LeastUpperBound(c, Vector(a, b), Position.internal),
    )

    assertInferenceSuccess(
      Assignment.upper(a, real),
      Assignment.upper(b, real),
      Assignment.upper(c, real),
    )(result)
  }

  it should "infer the result of an appends operation" in {
    val list = ListType(int)
    val element = new InferenceVariable(Some("element"))
    val newElement = real
    val combined = new InferenceVariable(Some("combined"))

    val result = infer(
      TypingJudgment.Equals(ListType(element), list, Position.internal),
      TypingJudgment.LeastUpperBound(combined, Vector(element, newElement), Position.internal),
    )

    assertInferenceSuccess(
      Assignment.fixed(element, int),
      Assignment.fixed(combined, real),
    )(result)
  }

}
