package lore.compiler.phases.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}

object StructTransformer {

  /**
    * Transforms the default value expressions of struct properties and assigns them to their 'defaultValue' fields.
    */
  def transform(struct: StructDefinition)(implicit registry: Registry, reporter: Reporter): Unit = {
    struct.properties.foreach { property =>
      property.defaultValue = property.defaultValueNode.map { node =>
        // Note that we pass the registry binding scope, since other properties of the struct should not be accessible in
        // default value expressions.
        val expression = ExpressionTransformer.transform(
          s"${struct.name}.${property.name}",
          node,
          property.tpe,
          registry.typeScope,
          registry.bindingScope
        )
        val callTarget = CallTarget.Dynamic(RuntimeNames.defaultValue(struct.tpe, property).name.name)
        StructPropertyDefinition.DefaultValue(expression, callTarget)
      }
    }
  }

}
