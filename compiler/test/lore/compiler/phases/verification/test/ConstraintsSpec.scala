package lore.compiler.phases.verification.test

import lore.compiler.phases.verification.{DeclaredTypeConstraints, EntityConstraints, MultiFunctionConstraints, ReturnConstraints, StructConstraints}
import lore.compiler.test.BaseSpec

class ConstraintsSpec extends BaseSpec {
  "constraints/entities" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/entities") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[DeclaredTypeConstraints.OwnedByMustBeSubtype], 7),
        ErrorSignature(classOf[EntityConstraints.EntityCannotOwnComponent], 9),
        ErrorSignature(classOf[StructConstraints.ComponentsShareSupertype], 22),
        ErrorSignature(classOf[StructConstraints.ComponentNotImplemented], 36),
      ))
    }
  }

  "constraints/output-types" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/output-types") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }

  "constraints/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors("test/constraints/returns") { errors =>
      assertErrorsMatchSignatures(errors, Vector(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 3),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 13),
      ))
    }
  }
}
