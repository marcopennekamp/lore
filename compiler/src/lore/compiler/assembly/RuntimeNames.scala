package lore.compiler.assembly

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.{StructProperty, StructSchema}

object RuntimeNames {

  object struct {
    def constructor(schema: StructSchema): NamePath = schema.name.appendToLastSegment("/construct")
    def `object`(schema: StructSchema): NamePath = schema.name.appendToLastSegment("/object")
    def defaultPropertyValue(property: StructProperty): NamePath = property.struct.name.appendToLastSegment(s"/default:${property.name}")
  }

  object globalVariable {
    def initializer(name: NamePath): NamePath = name.appendToLastSegment("/init")
    def initializer(variable: GlobalVariableDefinition): NamePath = initializer(variable.name)
  }

}
