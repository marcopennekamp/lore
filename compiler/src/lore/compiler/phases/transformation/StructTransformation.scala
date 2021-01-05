package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.phases.transpilation.TranspiledName
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.DynamicCallTarget
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}

object StructTransformation {

  /**
    * Transforms the default value expressions of struct properties and assigns them to their 'defaultValue' fields.
    */
  def transform(struct: StructDefinition)(implicit registry: Registry): Verification = {
    struct.properties.map { property =>
      val compiledExpression = property.defaultValueNode.map { node =>
        // Note that we pass the global variable scope, since other properties of the struct should not be accessible in
        // default value expressions.
        // TODO: Once we introduce parametric declared types, we have to provide the struct's type scope.
        ExpressionTransformation.transform(node, property.tpe, registry.typeScope, registry.variableScope)
      }.toCompiledOption

      compiledExpression.map { maybeExpression =>
        property.defaultValue = maybeExpression.map { expression =>
          StructPropertyDefinition.DefaultValue(expression, DynamicCallTarget(TranspiledName.defaultValue(struct.tpe, property).name, expression.tpe))
        }
      }
    }.simultaneous.verification
  }

}
