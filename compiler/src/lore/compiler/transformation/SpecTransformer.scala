package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.types.TupleType

object SpecTransformer {

  def transform(spec: SpecDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    spec.body.assign(
      ExpressionTransformer.transform(
        spec.node.body,
        TupleType.UnitType,
        registry.getTypeScope(spec.localModule),
        registry.getTermScope(spec.localModule),
        s"spec ${spec.localModule.globalModule.name} '${spec.description}'",
      )
    )
  }

}
