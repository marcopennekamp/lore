package lore.compiler.typing

import lore.compiler.test.BaseSpec

class SubtypingTypingSpec extends BaseSpec {

  private val fragmentBase = "typing"

  s"$fragmentBase/subtyping_errors" should "be compiled with typing errors" in {
    assertCompilationErrorMessages(s"$fragmentBase/subtyping_errors.lore")(
      ("`shape.prop` has the illegal type `A`. We expected the following type (or a subtype thereof): B.", 13),
      ("`Ox()` has the illegal type `Ox`. We expected the following type (or a subtype thereof): Fox.", 20),
      ("`test()` has the illegal type `Unit`. We expected the following type (or a subtype thereof): Real.", 25),
      ("`pi()` has the illegal type `Real`. We expected the following type (or a subtype thereof): String.", 31),
      ("`advance(fox)` has the illegal type `Animal`. We expected the following type (or a subtype thereof): Fox.", 40),
    )
  }

}
