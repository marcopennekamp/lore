package lore.compiler.inference

import lore.compiler.core.Position
import lore.compiler.types.TraitType

/**
  * Ensures basic properties of typing judgments and judgment resolvers.
  */
class BasicInferenceSpec extends InferenceSpec {

  "Inference" should "infer fixed bounds for Assign judgments" in {
    val A = new TraitType("A", Vector.empty)
    val B = new TraitType("B", Vector(A))
    val C = new TraitType("C", Vector(B))

    val lower = new InferenceVariable(Some("lower"))
    val upper = new InferenceVariable(Some("upper"))
    val both = new InferenceVariable(Some("both"))
    val fixed = new InferenceVariable(Some("fixed"))

    val target1 = new InferenceVariable(Some("target1"))
    val target2 = new InferenceVariable(Some("target2"))
    val target3 = new InferenceVariable(Some("target3"))
    val target4 = new InferenceVariable(Some("target4"))

    val result = infer(
      TypingJudgment.Subtypes(C, lower, Position.internal),
      TypingJudgment.Subtypes(upper, B, Position.internal),
      TypingJudgment.Subtypes(B, both, Position.internal),
      TypingJudgment.Subtypes(both, A, Position.internal),
      TypingJudgment.Equals(fixed, B, Position.internal),

      TypingJudgment.Assign(target1, lower, Position.internal),
      TypingJudgment.Assign(target2, upper, Position.internal),
      TypingJudgment.Assign(target3, both, Position.internal),
      TypingJudgment.Assign(target4, fixed, Position.internal),
    )

    assertInferenceSuccess(
      Assignment.lower(lower, C),
      Assignment.upper(upper, B),
      Assignment.assignment(both, B, A),
      Assignment.fixed(fixed, B),

      Assignment.fixed(target1, C),
      Assignment.fixed(target2, B),
      Assignment.fixed(target3, B),
      Assignment.fixed(target4, B),
    )(result)
  }

  it should "infer the best assignments from judgment (iv1, Int) :=: (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = infer(
      TypingJudgment.Equals((iv1, int), (real, iv2), Position.internal)
    )

    assertInferenceSuccess(
      Assignment.fixed(iv1, real),
      Assignment.fixed(iv2, int),
    )(result)
  }

  it should "infer the best assignments from judgment (iv1, Int) <:< (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = infer(
      TypingJudgment.Subtypes((iv1, int), (real, iv2), Position.internal)
    )

    assertInferenceSuccess(
      Assignment.upper(iv1, real),
      Assignment.lower(iv2, int),
    )(result)
  }

  it should "infer the best assignments from relationship `a :=: b :=: c` and `a :=: real`" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = infer(
      TypingJudgment.Equals(a, real, Position.internal),
      TypingJudgment.Equals(b, a, Position.internal),
      TypingJudgment.Equals(a, b, Position.internal),
      TypingJudgment.Equals(c, b, Position.internal),
      TypingJudgment.Equals(c, a, Position.internal),
    )

    assertInferenceSuccess(
      Assignment.fixed(a, real),
      Assignment.fixed(b, real),
      Assignment.fixed(c, real),
    )(result)
  }

}
