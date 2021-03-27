package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.types.StructType
import lore.compiler.utils.CollectionExtensions.VectorExtension

// TODO: Couldn't we technically put name resolution in expressions into its own phase? This might be worth doing
//       if the module system proves to be too complex to include in the transformation phase. Transformation is
//       quite complex already.

object TransformationPhase {

  def process(implicit registry: Registry): Verification = {
    val structDefinitions = registry.getTypeDeclarationsInOrder.map(_._2).filterType[StructType].map(_.definition)
    val functionDefinitions = registry.getMultiFunctions.values.toVector.flatMap(_.functions)
    (
      structDefinitions.map(StructTransformer.transform).simultaneous,
      functionDefinitions.map(FunctionTransformer.transform).simultaneous,
    ).simultaneous.verification
  }

}
