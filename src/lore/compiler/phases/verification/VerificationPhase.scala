package lore.compiler.phases.verification

import lore.compiler.Compilation._
import lore.compiler.Registry
import lore.compiler.phases.Phase
import lore.definitions.ClassDefinition

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

    // TODO: Type all function/constructor bodies.
    val withTypedFunctions = withVerifiedConstraints

    withTypedFunctions.verification
  }
}