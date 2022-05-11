package lore.compiler.constraints.traits

import lore.compiler.test.BaseSpec

class TraitConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/traits"

  s"$fragmentBase/invariant_inheritance" should "be compiled with various inheritance errors" in {
    assertCompilationErrorMessages(s"$fragmentBase/invariant_inheritance.lore")(
      ("The invariant parameter `A` of trait `X`, which `Z` inherits from, has multiple conflicting type arguments: Int, Real.", 5),
      ("The invariant parameter `A` of trait `X`, which `W` inherits from, has multiple conflicting type arguments: A, B.", 8),
    )
  }

  s"$fragmentBase/variance" should "be compiled with various variance errors" in {
    assertCompilationErrorMessages(s"$fragmentBase/variance.lore")(
      ("The covariant type variable B is in an illegal contravariant position.", 3),
      ("The contravariant type variable A is in an illegal covariant position.", 3),
      ("The covariant type variable B is in an illegal contravariant position.", 8),
      ("The contravariant type variable A is in an illegal covariant position.", 11),
      ("The covariant type variable A is in an illegal invariant position.", 17),
    )
  }
}
