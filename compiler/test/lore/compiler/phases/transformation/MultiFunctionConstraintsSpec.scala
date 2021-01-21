package lore.compiler.phases.transformation

import lore.compiler.test.BaseSpec

class MultiFunctionConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/transformation/constraints/multi-functions"

  "constraints/multi-functions/output-types" should "be compiled with 'incompatible output types' errors" in {
    assertCompilationErrors(s"$fragmentBase/output-types") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }
}
