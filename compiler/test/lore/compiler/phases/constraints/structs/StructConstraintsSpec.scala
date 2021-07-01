package lore.compiler.phases.constraints.structs

import lore.compiler.phases.constraints.StructConstraints
import lore.compiler.test.BaseSpec

class StructConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/constraints/structs"

  "constraints/structs/duplicate-property" should "be compiled with a 'duplicate property' error" in {
    assertCompilationErrors(s"$fragmentBase/duplicate-property.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[StructConstraints.DuplicateProperty], 2),
      ))
    }
  }

  "constraints/structs/inherited-shape" should "be compiled with various shape inheritance errors" in {
    assertCompilationErrors(s"$fragmentBase/inherited-shape.lore") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[StructConstraints.ShapeMissingProperty], 15),
        ErrorSignature(classOf[StructConstraints.ShapeInvalidPropertyType], 16),
        ErrorSignature(classOf[StructConstraints.ShapeInvalidPropertyType], 17),
      ))
    }
  }
}
