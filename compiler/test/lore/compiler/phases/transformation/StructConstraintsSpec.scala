package lore.compiler.phases.transformation

import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/transformation/constraints/structs"

  "constraints/structs/duplicate-property" should "be compiled with a 'duplicate property' error" in {
    assertCompilationErrors(s"$fragmentBase/duplicate-property") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[StructConstraints.DuplicateProperty], 2),
      ))
    }
  }

  "constraints/structs/inherited-shape" should "be compiled with various shape inheritance errors" in {
    assertCompilationErrors(s"$fragmentBase/inherited-shape") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[StructConstraints.ShapeMissingProperty], 15),
        ErrorSignature(classOf[StructConstraints.ShapeInvalidPropertyType], 16),
        ErrorSignature(classOf[StructConstraints.ShapeInvalidPropertyType], 17),
      ))
    }
  }
}
