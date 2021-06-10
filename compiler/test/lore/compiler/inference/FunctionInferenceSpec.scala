package lore.compiler.inference

class FunctionInferenceSpec extends InferenceSpec {

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
