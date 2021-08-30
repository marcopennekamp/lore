package lore.compiler.constraints.traits

import lore.compiler.test.BaseSpec

class TraitConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/traits"

  "constraints/traits/variance" should "be compiled with various variance errors" in {
    assertCompilationErrorMessages(s"$fragmentBase/variance.lore")(
      ("The covariant type variable B is in an illegal contravariant position.", 3),
      ("The contravariant type variable A is in an illegal covariant position.", 3),
      ("The covariant type variable B is in an illegal contravariant position.", 8),
      ("The contravariant type variable A is in an illegal covariant position.", 11),
      ("The covariant type variable A is in an illegal invariant position.", 17),
    )
  }
}
