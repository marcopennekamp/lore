package lore.compiler.assembly

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.StructSchema

object AsmRuntimeNames {

  object struct {
    def construct(schema: StructSchema): NamePath = schema.name.appendToLastSegment("/construct")
  }

  object globalVariable {
    def initializer(variable: GlobalVariableDefinition): NamePath = variable.name.appendToLastSegment("/init")
  }

}
