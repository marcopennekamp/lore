package lore.compiler.phases.typing

import lore.compiler.core.Position
import lore.compiler.types.{BasicType, ListType, TypeSpec}

class InferenceSpec extends TypeSpec {

  // TODO: Turn these into real tests.

  "Inference" should "infer a super easy problem" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = new Inference(Vector(
      TypingJudgment.Equals(a, BasicType.Real, Position.internal),
      TypingJudgment.Equals(b, a, Position.internal),
      TypingJudgment.Equals(a, b, Position.internal),
      TypingJudgment.Equals(c, b, Position.internal),
      TypingJudgment.Equals(c, a, Position.internal),
    ))(null).inferTypes()

    println(result)
  }

  it should "infer the result type of an addition" in {
    val a = new InferenceVariable(Some("a"))
    val b = new InferenceVariable(Some("b"))
    val c = new InferenceVariable(Some("c"))

    val result = new Inference(Vector(
      TypingJudgment.Subtypes(a, BasicType.Real, Position.internal),
      TypingJudgment.Subtypes(b, BasicType.Real, Position.internal),
      TypingJudgment.LeastUpperBound(c, Vector(a, b), Position.internal),
    ))(null).inferTypes()

    println(result)
  }

  it should "infer the result of an appends operation" in {
    val list = ListType(BasicType.Int)
    val element = new InferenceVariable(Some("element"))
    val newElement = BasicType.Real
    val combined = new InferenceVariable(Some("combined"))

    val result = new Inference(Vector(
      TypingJudgment.Equals(ListType(element), list, Position.internal),
      TypingJudgment.LeastUpperBound(combined, Vector(element, newElement), Position.internal),
    ))(null).inferTypes()

    println(result)
  }

}
