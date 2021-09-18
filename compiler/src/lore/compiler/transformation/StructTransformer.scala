package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.transpilation.RuntimeNames

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
          node,
          property.tpe,
          struct.schema.getTypeScope(registry.getTypeScope(struct.localModule)),
          registry.getBindingScope(struct.localModule),
          s"${struct.name}.${property.name}",
        )
        val callTarget = CallTarget.Dynamic(RuntimeNames.struct.defaultValue(struct.schema, property).toString)
        StructPropertyDefinition.DefaultValue(expression, callTarget)
      }
    }
  }

}
