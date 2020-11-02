package lore.compiler.phases.transformation

import lore.compiler.test.BaseSpec

class ConstraintsSpec extends BaseSpec {
  private val fragmentBase = "phases/transformation/constraints"

  // TODO: Cover other constraints so that at least all possible constraint errors are tested with at least one
  //       test source each.

  "constraints/entities" should "be compiled with various errors" in {
    assertCompilationErrors(s"$fragmentBase/entities") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[DeclaredTypeConstraints.OwnedByMustBeSubtype], 7),
        ErrorSignature(classOf[EntityConstraints.EntityCannotOwnComponent], 9),
        ErrorSignature(classOf[StructConstraints.ComponentsShareSupertype], 22),
        ErrorSignature(classOf[StructConstraints.ComponentNotImplemented], 36),
      ))
    }
  }

  "constraints/output-types" should "be compiled with various errors" in {
    assertCompilationErrors(s"$fragmentBase/output-types") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }

  "constraints/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors(s"$fragmentBase/returns") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 3),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 13),
      ))
    }
  }
}
