package lore.compiler.constraints.structs

import lore.compiler.constraints.VarianceConstraints
import lore.compiler.feedback.{StructFeedback, TypingFeedback}
import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "constraints/structs"

  s"$fragmentBase/companion_modules" should "be compiled with various companion module errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/companion_modules.lore")(
      (classOf[StructFeedback.CompanionModuleExpected], 4),
      (classOf[StructFeedback.Object.MemberNameTaken], 15),
      (classOf[StructFeedback.Object.MemberNameTaken], 17),
      (classOf[StructFeedback.Object.CompanionModuleExpected], 27),
    )
  }

  s"$fragmentBase/duplicate_property" should "be compiled with a 'duplicate property' error" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicate_property.lore")(
      (classOf[StructFeedback.DuplicateProperty], 2),
    )
  }

  s"$fragmentBase/inherited_shape" should "be compiled with various shape inheritance errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/inherited_shape.lore")(
      (classOf[StructFeedback.Shape.MissingProperty], 15),
      (classOf[StructFeedback.Shape.InvalidPropertyType], 16),
      (classOf[StructFeedback.Shape.InvalidPropertyType], 17),
    )
  }

  s"$fragmentBase/object_property_values" should "be compiled with 'missing default value' errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/object_property_values.lore")(
      (classOf[StructFeedback.Object.MissingDefault], 2),
      (classOf[StructFeedback.Object.MissingDefault], 8),
    )
  }

  s"$fragmentBase/open_type_parameters" should "be compiled with various open type parameter errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/open_type_parameters.lore")(
      (classOf[StructFeedback.OpenTypeParameter.CovarianceRequired], 2),
      (classOf[StructFeedback.OpenTypeParameter.NotUniquelyDeducible], 7),
      (classOf[TypingFeedback.InvalidVariance], 14),
      (classOf[StructFeedback.OpenTypeParameter.NotUniquelyDeducible], 14),
      (classOf[TypingFeedback.InvalidVariance], 19),
      (classOf[StructFeedback.OpenTypeParameter.MutableProperty], 19),
    )
  }

  s"$fragmentBase/variance" should "be compiled with various variance errors" in {
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
