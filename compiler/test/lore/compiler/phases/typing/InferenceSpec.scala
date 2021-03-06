package lore.compiler.phases.typing

import lore.compiler.core.Position
import lore.compiler.phases.typing.inference.{InferenceResolution, InferenceVariable, TypingJudgment}
import lore.compiler.types.{BasicType, ListType, TypeSpec}

class InferenceSpec extends TypeSpec {

  // TODO: Turn these into real tests.

  "Inference" should "infer a super easy problem" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = InferenceResolution.infer(Vector(
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

    val result = InferenceResolution.infer(Vector(
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

    val result = InferenceResolution.infer(Vector(
      TypingJudgment.Equals(ListType(element), list, Position.internal),
      TypingJudgment.LeastUpperBound(combined, Vector(element, newElement), Position.internal),
    ))(null)

    println(result)
  }

  // TODO: This should work.
  it should "infer the result of a judgment (iv1, Int) :=: (Real, iv2)" in {
    val iv1 = new InferenceVariable(Some("iv1"))
    val iv2 = new InferenceVariable(Some("iv2"))

    val result = InferenceResolution.infer(Vector(
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
