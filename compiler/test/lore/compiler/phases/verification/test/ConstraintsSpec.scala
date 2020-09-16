package lore.compiler.phases.verification.test

import lore.compiler.phases.verification.{StructConstraints, MultiFunctionConstraints, ReturnConstraints}
import lore.compiler.semantics.Registry
import lore.compiler.test.BaseSpec

class ConstraintsSpec extends BaseSpec {
  /* "constraints/classes/constructors" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/classes/constructors") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[ConstructorConstraints.ContinuationRequired], 8),
        ErrorSignature(classOf[ConstructorConstraints.CyclicContinuations], 12),
      ))
    }
  } */

  "constraints/classes/entities" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/classes/entities") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[StructConstraints.OwnedByMustBeSubtype], 7),
        ErrorSignature(classOf[StructConstraints.ClassCannotOwnComponent], 12),
        ErrorSignature(classOf[StructConstraints.ClassMayNotExtendEntity], 16),
        ErrorSignature(classOf[StructConstraints.MemberAlreadyExistsInSuperclass], 23),
        ErrorSignature(classOf[StructConstraints.ComponentMustSubtypeOverriddenComponent], 40),
        ErrorSignature(classOf[StructConstraints.OverriddenComponentDoesNotExist], 42),
        ErrorSignature(classOf[StructConstraints.OverriddenComponentDoesNotExist], 44),
        ErrorSignature(classOf[Registry.TypeNotFound], 44),
        ErrorSignature(classOf[StructConstraints.ComponentsShareSupertype], 48),
        ErrorSignature(classOf[StructConstraints.OverriddenComponentDoesNotExist], 65),
      ))
    }
  }

  "constraints/output-types" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/output-types") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 8),
        ErrorSignature(classOf[MultiFunctionConstraints.IncompatibleOutputTypes], 11),
      ))
    }
  }

  "constraints/returns" should "be compiled with 'dead code' and 'impossible return' errors" in {
    assertCompilationErrors("test/constraints/returns") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[ReturnConstraints.ImpossibleReturn], 3),
        ErrorSignature(classOf[ReturnConstraints.DeadCode], 13),
      ))
    }
  }
}
