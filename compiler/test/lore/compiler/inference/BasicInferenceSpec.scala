package lore.compiler.inference

import lore.compiler.core.Position

/**
  * Ensures basic properties of typing judgments and judgment resolvers.
  */
class BasicInferenceSpec extends InferenceSpec {

  // TODO: Add a test that ensures that Assign always produces fixed bounds.

  it should "infer the best assignments from judgment (iv1, Int) :=: (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals((iv1, int), (real, iv2), Position.internal)
    ))(null)

    assertInferenceSuccess(
      Assignment.fixed(iv1, real),
      Assignment.fixed(iv2, int),
    )(result)
  }

  it should "infer the best assignments from judgment (iv1, Int) <:< (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = Inference.infer(Vector(
      TypingJudgment.Subtypes((iv1, int), (real, iv2), Position.internal)
    ))(null)

    assertInferenceSuccess(
      Assignment.upper(iv1, real),
      Assignment.lower(iv2, int),
    )(result)
  }

  "Inference" should "infer the best assignments from relationship `a :=: b :=: c` and `a :=: real`" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals(a, real, Position.internal),
      TypingJudgment.Equals(b, a, Position.internal),
      TypingJudgment.Equals(a, b, Position.internal),
      TypingJudgment.Equals(c, b, Position.internal),
      TypingJudgment.Equals(c, a, Position.internal),
    ))(null)

    assertInferenceSuccess(
      Assignment.fixed(a, real),
      Assignment.fixed(b, real),
      Assignment.fixed(c, real),
    )(result)
  }

}
