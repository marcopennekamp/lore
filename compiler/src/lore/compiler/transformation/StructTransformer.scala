package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.types.StructSchema

object StructTransformer {

  /**
    * Transforms the default value expressions of struct properties and assigns them to their 'defaultValue' fields.
    */
  def transform(struct: StructSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    struct.properties.foreach { property =>
      property.defaultValue = property.defaultValueNode.map { node =>
        // Note that we pass the registry binding scope, since other properties of the struct should not be accessible
        // in default value expressions.
        ExpressionTransformer.transform(
          node,
          property.tpe,
          struct.getTypeScope(registry.getTypeScope(struct.localModule)),
          registry.getTermScope(struct.localModule),
          s"${struct.name}.${property.name}",
        )
      }
    }
  }

}
