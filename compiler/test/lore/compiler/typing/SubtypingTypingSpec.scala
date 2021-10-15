package lore.compiler.typing

import lore.compiler.test.BaseSpec

class SubtypingTypingSpec extends BaseSpec {

  "Typing" should "reject `subtyping-errors.lore` with the correct errors" in {
    assertCompilationErrorMessages("typing/subtyping-errors.lore")(
      ("This expression has the illegal type A. We expected the following type (or a subtype thereof): B.", 9),
      ("This expression has the illegal type Real. We expected the following type (or a subtype thereof): Int.", 16),
      ("This expression has the illegal type Unit. We expected the following type (or a subtype thereof): Real.", 21),
      ("This expression has the illegal type Real. We expected the following type (or a subtype thereof): String.", 27),
      ("This expression has the illegal type Real. We expected the following type (or a subtype thereof): Int.", 35),
    )
  }

}
