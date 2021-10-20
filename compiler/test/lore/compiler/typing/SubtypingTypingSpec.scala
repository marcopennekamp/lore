package lore.compiler.typing

import lore.compiler.test.BaseSpec

class SubtypingTypingSpec extends BaseSpec {

  "Typing" should "reject `subtyping-errors.lore` with the correct errors" in {
    assertCompilationErrorMessages("typing/subtyping-errors.lore")(
      ("This expression has the illegal type A. We expected the following type (or a subtype thereof): B.", 13),
      ("A construction of Ox cannot result in expected type Fox, because Fox cannot be specialized to Ox. Most likely, Ox is not a subtype of Fox.", 20),
      ("This expression has the illegal type Unit. We expected the following type (or a subtype thereof): Number.", 25),
      ("This expression has the illegal type Number. We expected the following type (or a subtype thereof): String.", 31),
      ("This expression has the illegal type Animal. We expected the following type (or a subtype thereof): Fox.", 40),
    )
  }

}
