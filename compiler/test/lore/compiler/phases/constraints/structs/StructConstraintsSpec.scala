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
}
