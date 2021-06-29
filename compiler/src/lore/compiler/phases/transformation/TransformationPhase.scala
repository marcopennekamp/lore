package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.types.StructType
import lore.compiler.utils.CollectionExtensions.VectorExtension

object TransformationPhase {

  def process(implicit registry: Registry): Verification = {
    val structDefinitions = registry.typesInOrder.map(_._2).filterType[StructType].map(_.definition)
    val functionDefinitions = registry.multiFunctions.values.toVector.flatMap(_.functions)
    (
      structDefinitions.map(StructTransformer.transform).simultaneous,
      functionDefinitions.map(FunctionTransformer.transform).simultaneous,
    ).simultaneous.verification
  }

}
