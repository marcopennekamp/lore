package lore.compiler.phases.verification

import lore.compiler.core.Compilation._
import lore.compiler.core.Registry
import lore.compiler.phases.Phase
import lore.compiler.structures.ClassDefinition
import lore.compiler.utils.CollectionExtensions._

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
            FunctionVerification.verifyTypeTransform(function)
          }.simultaneous
        }.simultaneous,
        registry.getTypeDefinitions.values.toList.filterType[ClassDefinition].map { definition =>
          definition.constructors.map { constructor =>
            FunctionVerification.verifyTypeTransform(constructor, definition)
          }.simultaneous
        }.simultaneous,
      ).simultaneous
    }

    withTypedFunctions.verification
  }
}
