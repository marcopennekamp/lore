package lore.compiler.phases.typing

import lore.compiler.core.Position
import lore.compiler.phases.typing.inference.{Inference, InferenceVariable, TypingJudgment}
import lore.compiler.types.{BasicType, ListType, ShapeType, TraitType, TypeSpec}

class InferenceSpec extends TypeSpec {

  // TODO: Turn these into real tests.

  "Inference" should "infer a super easy problem" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals(a, BasicType.Real, Position.internal),
      TypingJudgment.Equals(b, a, Position.internal),
      TypingJudgment.Equals(a, b, Position.internal),
      TypingJudgment.Equals(c, b, Position.internal),
      TypingJudgment.Equals(c, a, Position.internal),
    ))(null)

    println(result)
  }

  it should "infer the result type of an addition" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = Inference.infer(Vector(
      TypingJudgment.Subtypes(a, BasicType.Real, Position.internal),
      TypingJudgment.Subtypes(b, BasicType.Real, Position.internal),
      TypingJudgment.LeastUpperBound(c, Vector(a, b), Position.internal),
    ))(null)

    println(result)
  }

  it should "infer the result of an appends operation" in {
    val list = ListType(BasicType.Int)
    val element = new InferenceVariable(Some("element"))
    val newElement = BasicType.Real
    val combined = new InferenceVariable(Some("combined"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals(ListType(element), list, Position.internal),
      TypingJudgment.LeastUpperBound(combined, Vector(element, newElement), Position.internal),
    ))(null)

    println(result)
  }

  it should "reject an incorrectly narrowed member type (test:inference:0001)" in {
    val C = new TraitType("C", Vector.empty)
    val B = new TraitType("B", Vector(C))

    val p = new InferenceVariable(Some("p"))
    val x = new InferenceVariable(Some("x"))

    val result = Inference.infer(Vector(
      TypingJudgment.Subtypes(x, B, Position.internal),
      TypingJudgment.Equals(p, ShapeType("m" -> C), Position.internal),
      TypingJudgment.MemberAccess(x, p, "m", Position.internal),
    ))(null)

    println(result)
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

    println(result)
  }

  it should "infer the result of a judgment (iv1, Int) :=: (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = Inference.infer(Vector(
      TypingJudgment.Equals((iv1, BasicType.Int), (BasicType.Real, iv2), Position.internal)
    ))(null)

    println(result)
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
