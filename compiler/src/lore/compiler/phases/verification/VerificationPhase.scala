package lore.compiler.phases.verification

import lore.compiler.core.Compilation._
import lore.compiler.core.Phase
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.ClassDefinition
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

    // Verify, type and transform all function/constructor bodies.
    val withTransformedFunctions = withVerifiedConstraints.flatMap { _ =>
      (
        registry.getMultiFunctions.values.toList.map { mf =>
          mf.functions.map { function =>
            FunctionTransformation.transform(function)
          }.simultaneous
        }.simultaneous,
        registry.getTypeDefinitions.values.toList.filterType[ClassDefinition].map { definition =>
          definition.constructors.map { constructor =>
            FunctionTransformation.transform(constructor, definition)
          }.simultaneous
        }.simultaneous,
      ).simultaneous
    }

    withTransformedFunctions.verification
  }
}
