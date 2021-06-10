package lore.compiler.phases.transformation

import lore.compiler.core.{Compilation, Errors, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{Inference, InferenceBounds, InferenceVariable, TypingJudgment}
import lore.compiler.types._
import org.scalatest.Assertion

class InferenceSpec extends TypeSpec {

  def assertSuccess(assignments: (InferenceVariable, InferenceBounds)*)(result: Compilation[Assignments]): Assertion = {
    result shouldEqual Compilation.succeed(assignments.toMap)
  }

  def assertFailure(error: Feedback.Error)(result: Compilation[Assignments]): Assertion = {
    result shouldEqual Compilation.fail(error)
  }

  def assertFailureDisregardingErrors(result: Compilation[Assignments]): Assertion = {
    result should matchPattern { case Errors(_, _) => }
  }

  object Assignment {
    def lower(iv: InferenceVariable, lowerBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, lowerBound, any)
    def upper(iv: InferenceVariable, upperBound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, nothing, upperBound)
    def fixed(iv: InferenceVariable, bound: Type): (InferenceVariable, InferenceBounds) = assignment(iv, bound, bound)
    def assignment(iv: InferenceVariable, lowerBound: Type, upperBound: Type): (InferenceVariable, InferenceBounds) = iv -> InferenceBounds(iv, lowerBound, upperBound)
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

    assertSuccess(
      Assignment.fixed(a, real),
      Assignment.fixed(b, real),
      Assignment.fixed(c, real),
    )(result)
  }

  it should "infer the result type of an addition" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = Inference.infer(Vector(
      TypingJudgment.Subtypes(a, real, Position.internal),
      TypingJudgment.Subtypes(b, real, Position.internal),
      TypingJudgment.LeastUpperBound(c, Vector(a, b), Position.internal),
    ))(null)

    assertSuccess(
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

    val result = Inference.infer(Vector(
      TypingJudgment.Equals(ListType(element), list, Position.internal),
      TypingJudgment.LeastUpperBound(combined, Vector(element, newElement), Position.internal),
    ))(null)

    assertSuccess(
      Assignment.fixed(element, int),
      Assignment.fixed(combined, real),
    )(result)
  }

  // TODO: Add a test that ensures that Assign always produces fixed bounds.

  it should "infer the best assignments from judgment (iv1, Int) :=: (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals((iv1, int), (real, iv2), Position.internal)
    ))(null)

    assertSuccess(
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

    assertSuccess(
      Assignment.upper(iv1, real),
      Assignment.lower(iv2, int),
    )(result)
  }

  it should "reject an incorrect subtyping relationship with a member type (test:inference:0001)" in {
    val C = new TraitType("C", Vector.empty)
    val B = new TraitType("B", Vector(C))

    val p = new InferenceVariable(Some("p"))
    val x = new InferenceVariable(Some("x"))

    // This test case ensures that a member type cannot be narrowed further after it is decided. `x` should not be
    // typed as `B`, because we don't know whether `p`'s member is just a `C` or may always be a `B`.
    val memberAccess = TypingJudgment.MemberAccess(x, p, "m", Position.internal)
    val result = Inference.infer(Vector(
      TypingJudgment.Subtypes(p, ShapeType("m" -> C), Position.internal),
      memberAccess,
      TypingJudgment.Subtypes(x, B, Position.internal),
    ))(null)

    assertFailureDisregardingErrors(result)
  }

  it should "infer a prematurely narrowed member type correctly (test:inference:0002)" in {
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
    val result = Inference.infer(Vector(
      TypingJudgment.Equals(z, B, Position.internal),
      TypingJudgment.Subtypes(x, B, Position.internal),
      TypingJudgment.Subtypes(p, ShapeType("m" -> C), Position.internal),
      TypingJudgment.Subtypes(p, ShapeType("m" -> z), Position.internal),
      TypingJudgment.MemberAccess(x, p, "m", Position.internal),
    ))(null)

    assertSuccess(
      Assignment.upper(p, ShapeType("m" -> B)),
      Assignment.fixed(x, B),
      Assignment.fixed(z, B),
    )(result)
  }

  it should "infer a complex list of functions" in {
    // TODO: Make sure that the following can be inferred:
    //          let a: [Thing => Boolean] = [v => v.x == 4]
    //          let b: [Thing => Boolean] = a :+ (v => v.x > 3.4)  <-- Should infer v: Thing.
    //       In Scala:
    case class Thing(x: Int)
    val a: Vector[Thing => Boolean] = Vector(v => v.x == 4)
    val b: Vector[Thing => Boolean] = a :+ (v => v.x > 3)

    // TODO: ...
  }

}
