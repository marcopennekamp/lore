package lore.compiler.constraints.structs

import lore.compiler.constraints.VarianceConstraints
import lore.compiler.feedback.StructFeedback
import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/structs"

  "constraints/structs/companion-modules" should "be compiled with various companion module errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/companion-modules.lore")(
      (classOf[StructFeedback.CompanionModuleExpected], 4),
      (classOf[StructFeedback.Object.MemberNameTaken], 15),
      (classOf[StructFeedback.Object.MemberNameTaken], 17),
      (classOf[StructFeedback.Object.CompanionModuleExpected], 27),
    )
  }

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

  "constraints/structs/object-property-values" should "be compiled with 'missing default value' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/object-property-values.lore")(
      (classOf[StructFeedback.Object.MissingDefault], 2),
      (classOf[StructFeedback.Object.MissingDefault], 8),
    )
  }

  "constraints/structs/open-type-parameters" should "be compiled with various open type parameter errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/open-type-parameters.lore")(
      (classOf[StructFeedback.OpenTypeParameter.CovarianceRequired], 2),
      (classOf[StructFeedback.OpenTypeParameter.NotUniquelyDeducible], 7),
      (classOf[VarianceConstraints.InvalidVariance], 14),
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
      ("The contravariant type variable A is in an illegal invariant position.", 13),
      ("The covariant type variable B is in an illegal invariant position.", 15),
      ("The contravariant type variable A is in an illegal invariant position.", 19),
      ("The contravariant type variable A is in an illegal invariant position.", 21),
      ("The covariant type variable A is in an illegal invariant position.", 27),
    )
  }
}
