package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableTransformer {

  def transform(variable: GlobalVariableDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    variable.value = ExpressionTransformer.transform(
      variable.valueNode,
      variable.tpe,
      registry.getTypeScope(variable.localModule),
      registry.getBindingScope(variable.localModule),
      variable.name.toString,
    )
  }

}
