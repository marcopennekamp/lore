package lore.compiler.phases.verification.test

import lore.compiler.phases.verification.{ClassConstraints, ConstructorConstraints, MultiFunctionConstraints, ReturnConstraints}
import lore.compiler.semantics.Registry
import lore.compiler.test.BaseSpec

class ConstraintsSpec extends BaseSpec {
  "constraints/classes/constructors" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/classes/constructors") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[ConstructorConstraints.ContinuationRequired], 8),
        ErrorSignature(classOf[ConstructorConstraints.CyclicContinuations], 12),
      ))
    }
  }

  "constraints/classes/entities" should "be compiled with various errors" in {
    assertCompilationErrors("test/constraints/classes/entities") { errors =>
      assertErrorsMatchSignatures(errors, List(
        ErrorSignature(classOf[ClassConstraints.OwnedByMustBeSubtype], 7),
        ErrorSignature(classOf[ClassConstraints.ClassCannotOwnComponent], 12),
        ErrorSignature(classOf[ClassConstraints.ClassMayNotExtendEntity], 16),
        ErrorSignature(classOf[ClassConstraints.MemberAlreadyExistsInSuperclass], 23),
        ErrorSignature(classOf[ClassConstraints.ComponentMustSubtypeOverriddenComponent], 40),
        ErrorSignature(classOf[ClassConstraints.OverriddenComponentDoesNotExist], 42),
        ErrorSignature(classOf[ClassConstraints.OverriddenComponentDoesNotExist], 44),
        ErrorSignature(classOf[Registry.TypeNotFound], 44),
        ErrorSignature(classOf[ClassConstraints.ComponentsShareSuperclass], 48),
        ErrorSignature(classOf[ClassConstraints.OverriddenComponentDoesNotExist], 65),
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
