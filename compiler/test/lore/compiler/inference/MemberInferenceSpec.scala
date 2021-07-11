package lore.compiler.inference

import lore.compiler.core.Position
import lore.compiler.types.{ShapeType, TraitType}

class MemberInferenceSpec extends InferenceSpec {

  "Inference" should "reject an incorrect subtyping relationship with a member type (test:inference:member:0001)" in {
    val C = new TraitType("C", Vector.empty)
    val B = new TraitType("B", Vector(C))

    val p = new InferenceVariable(Some("p"))
    val x = new InferenceVariable(Some("x"))

    // This test case ensures that a member type cannot be narrowed further after it is decided. `x` should not be
    // typed as `B`, because we don't know whether `p`'s member is just a `C` or may always be a `B`.
    val memberAccess = TypingJudgment.MemberAccess(x, p, "m", Position.internal)
    val result = infer(
      TypingJudgment.Subtypes(p, ShapeType("m" -> C), Position.internal),
      memberAccess,
      TypingJudgment.Subtypes(x, B, Position.internal),
    )

    assertInferenceFailureDisregardingErrors(result)
  }

  it should "infer a prematurely narrowed member type correctly (test:inference:member:0002)" in {
    val C = new TraitType("C", Vector.empty)
    val B = new TraitType("B", Vector(C))

    val p = new InferenceVariable(Some("p"))
    val x = new InferenceVariable(Some("x"))
    val z = new InferenceVariable(Some("z"))

    // The idea here is that the algorithm will type `x` as `B` right away, because it is most straight-forward. Then
    // the next straight-forward step is typing `x` as `C`, because we have a member access `p.m` which is `C` at
    // first. If the algorithm rejects these judgments at that stage, it is false: we later narrow p's upper bound to
    // `{ m: B }`, which means that `x` will now be typed as `B`. Ultimately, the judgments are perfectly legal. The
    // point of this test is to ensure that the compiler doesn't make the mistake of failing prematurely.
    val result = infer(
      TypingJudgment.Equals(z, B, Position.internal),
      TypingJudgment.Subtypes(x, B, Position.internal),
      TypingJudgment.Subtypes(p, ShapeType("m" -> C), Position.internal),
      TypingJudgment.Subtypes(p, ShapeType("m" -> z), Position.internal),
      TypingJudgment.MemberAccess(x, p, "m", Position.internal),
    )

    assertInferenceSuccess(
      Assignment.upper(p, ShapeType("m" -> B)),
      Assignment.fixed(x, B),
      Assignment.fixed(z, B),
    )(result)
  }

}
