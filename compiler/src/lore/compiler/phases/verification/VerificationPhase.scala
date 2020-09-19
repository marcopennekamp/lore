package lore.compiler.phases.verification

import lore.compiler.core.Compilation._
import lore.compiler.core.Phase
import lore.compiler.semantics.Registry

// TODO: Rename to TransformationPhase?
class VerificationPhase()(implicit registry: Registry) extends Phase[Unit] {
  override def result: Verification = {
    val withVerifiedConstraints = (
      registry.getTypeDefinitions.values.toVector.map(DeclaredTypeConstraints.verify).simultaneous,
      registry.getMultiFunctions.values.toVector.map(MultiFunctionConstraints.verify).simultaneous,
    ).simultaneous

    val withTransformedFunctions = withVerifiedConstraints.flatMap { _ =>
      registry.getMultiFunctions.values.toVector.map { mf =>
        mf.functions.map(FunctionTransformation.transform).simultaneous
      }.simultaneous
    }

    withTransformedFunctions.verification
  }
}
