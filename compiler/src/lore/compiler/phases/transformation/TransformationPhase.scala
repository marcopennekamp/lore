package lore.compiler.phases.transformation

import lore.compiler.core.Compilation._
import lore.compiler.core.Phase
import lore.compiler.semantics.Registry
import lore.compiler.types.StructType
import lore.compiler.utils.CollectionExtensions.VectorExtension

class TransformationPhase()(implicit registry: Registry) extends Phase[Unit] {
  override def result: Verification = {
    val withVerifiedConstraints = (
      registry.getTypeDefinitions.values.toVector.map(DeclaredTypeConstraints.verify).simultaneous,
      registry.getMultiFunctions.values.toVector.map(MultiFunctionConstraints.verify).simultaneous,
    ).simultaneous

    val withTransformedStructs = withVerifiedConstraints.flatMap { _ =>
      registry.getTypeDeclarationsInOrder.map(_._2).filterType[StructType].map { structType =>
        StructTransformation.transform(structType.definition)
      }.simultaneous
    }

    val withTransformedFunctions = withTransformedStructs.flatMap { _ =>
      registry.getMultiFunctions.values.toVector.map { mf =>
        mf.functions.map(FunctionTransformation.transform).simultaneous
      }.simultaneous
    }

    withTransformedFunctions.verification
  }
}
