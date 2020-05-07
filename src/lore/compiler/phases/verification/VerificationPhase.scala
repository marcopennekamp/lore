package lore.compiler.phases.verification

import lore.compiler.Compilation._
import lore.compiler.Registry
import lore.compiler.phases.Phase
import lore.definitions.ClassDefinition
import lore.utils.CollectionExtensions._

class VerificationPhase()(implicit registry: Registry) extends Phase[Unit] {
  override def result: Verification = {
    val withVerifiedConstraints = (
      // Verify declared type constraints.
      registry.getTypeDefinitions.values.map {
        case definition: ClassDefinition => ClassConstraints.verify(definition)
        case _ => Verification.succeed
      }.toList.simultaneous,
      // Verify multi-function constraints.
      registry.getMultiFunctions.values.map(MultiFunctionConstraints.verify).toList.simultaneous,
    ).simultaneous

    // Type all function/constructor bodies.
    val withTypedFunctions = withVerifiedConstraints.flatMap { _ =>
      (
        registry.getMultiFunctions.values.toList.map { mf =>
          mf.functions.map { function =>
            FunctionVerification.verifyFunction(function, None)
          }.simultaneous
        }.simultaneous,
        registry.getTypeDefinitions.values.toList.filterType[ClassDefinition].map { definition =>
          definition.constructors.map { constructor =>
            FunctionVerification.verifyFunction(constructor, Some(definition))
          }.simultaneous
        }.simultaneous,
      ).simultaneous
    }

    withTypedFunctions.verification
  }
}
