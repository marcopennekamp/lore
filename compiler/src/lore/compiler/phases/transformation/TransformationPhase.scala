package lore.compiler.phases.transformation

import lore.compiler.core.Compilation._
import lore.compiler.semantics.Registry
import lore.compiler.types.StructType
import lore.compiler.utils.CollectionExtensions.VectorExtension

object TransformationPhase {
  def process(implicit registry: Registry): Verification = {
    val withVerifiedConstraints = (
      registry.getTypeDefinitions.values.toVector.map(DeclaredTypeConstraints.verify).simultaneous,
      registry.getMultiFunctions.values.toVector.map(MultiFunctionConstraints.verify).simultaneous,
    ).simultaneous

    val withTransformedStructs = withVerifiedConstraints.flatMap { _ =>
      registry.getTypeDeclarationsInOrder.map(_._2).filterType[StructType].map { structType =>
        StructTransformer.transform(structType.definition)
      }.simultaneous
    }

    val withTransformedFunctions = withTransformedStructs.flatMap { _ =>
      registry.getMultiFunctions.values.toVector.map { mf =>
        mf.functions.map(FunctionTransformer.transform).simultaneous
      }.simultaneous
    }

    withTransformedFunctions.verification
  }
}
