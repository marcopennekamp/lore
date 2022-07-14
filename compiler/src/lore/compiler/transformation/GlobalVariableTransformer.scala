package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.variables.GlobalVariableDefinition

object GlobalVariableTransformer {

  def transform(variable: GlobalVariableDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    variable.value.assign(
      ExpressionTransformer.transform(
        variable.node.value,
        variable.tpe,
        registry.getTypeScope(variable.localModule),
        registry.getTermScope(variable.localModule),
        variable.name.toString,
      )
    )
  }

}
