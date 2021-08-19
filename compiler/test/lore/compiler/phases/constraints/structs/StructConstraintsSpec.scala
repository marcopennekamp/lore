package lore.compiler.phases.constraints.structs

import lore.compiler.phases.constraints.StructConstraints
import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints/structs"

  "constraints/structs/duplicate-property" should "be compiled with a 'duplicate property' error" in {
    assertCompilationErrorSignatures(s"$fragmentBase/duplicate-property.lore")(
      (classOf[StructConstraints.DuplicateProperty], 2),
    )
  }

  "constraints/structs/inherited-shape" should "be compiled with various shape inheritance errors" in {
    assertCompilationErrorSignatures(s"$fragmentBase/inherited-shape.lore")(
      (classOf[StructConstraints.ShapeMissingProperty], 15),
      (classOf[StructConstraints.ShapeInvalidPropertyType], 16),
      (classOf[StructConstraints.ShapeInvalidPropertyType], 17),
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
