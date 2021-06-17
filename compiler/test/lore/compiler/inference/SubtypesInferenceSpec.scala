package lore.compiler.inference

class SubtypesInferenceSpec extends InferenceSpec {

  "Inference" should "reject `subtyping-errors.lore` with the correct errors" in {
    assertCompilationErrors("inference/subtyping-errors.lore") { errors =>
      errors.map(e => (e.message, e.position.line)) shouldEqual Vector(
        ("This expression has the illegal type A. We expected the following type (or a subtype thereof): B.", 9),
        ("This expression has the illegal type Real. We expected the following type (or a subtype thereof): Int.", 16),
        ("This expression has the illegal type Unit. We expected the following type (or a subtype thereof): Real.", 21),
        //("This expression has the illegal type String. We expected the following type (or a subtype thereof): Real.", 34),
        ("This expression has the illegal type { number: Real }. We expected the following type (or a subtype thereof): { number: Int }.", 42),
      )
    }
  }

}
