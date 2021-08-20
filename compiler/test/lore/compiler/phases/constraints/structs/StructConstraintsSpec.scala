package lore.compiler.phases.constraints.structs

import lore.compiler.feedback.StructFeedback
import lore.compiler.phases.constraints.VarianceConstraints
import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints/structs"

  "constraints/structs/duplicate-property" should "be compiled with a 'duplicate property' error" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicate-property.lore")(
      (classOf[StructFeedback.DuplicateProperty], 2),
    )
  }

  "constraints/structs/inherited-shape" should "be compiled with various shape inheritance errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/inherited-shape.lore")(
      (classOf[StructFeedback.Shape.MissingProperty], 15),
      (classOf[StructFeedback.Shape.InvalidPropertyType], 16),
      (classOf[StructFeedback.Shape.InvalidPropertyType], 17),
    )
  }

  "constraints/structs/open-type-parameters" should "be compiled with various open type parameter errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/open-type-parameters.lore")(
      (classOf[StructFeedback.OpenTypeParameter.CovarianceRequired], 2),
      (classOf[StructFeedback.OpenTypeParameter.NotUniquelyDeducible], 7),
      (classOf[StructFeedback.OpenTypeParameter.NotUniquelyDeducible], 14),
      (classOf[VarianceConstraints.InvalidVariance], 19),
      (classOf[StructFeedback.OpenTypeParameter.MutableProperty], 19),
    )
  }

  "constraints/structs/variance" should "be compiled with various variance errors" in {
    assertCompilationErrorMessages(s"$fragmentBase/variance.lore")(
      ("The covariant type variable B is in an illegal contravariant position.", 5),
      ("The contravariant type variable A is in an illegal covariant position.", 5),
      ("The covariant type variable B is in an illegal contravariant position.", 10),
      ("The contravariant type variable A is in an illegal covariant position.", 10),
      ("The type variable A must be invariant.", 13),
      ("The type variable B must be invariant.", 15),
      ("The type variable A must be invariant.", 19),
    )
  }
}
