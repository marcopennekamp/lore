package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructDefinition

object StructTransformation {

  /**
    * Transforms the default value expressions of struct members and assigns them to their 'defaultValue' fields.
    */
  def transform(struct: StructDefinition)(implicit registry: Registry): Verification = {
    struct.members.map { member =>
      val compiledDefaultValue = member.defaultValueNode.map { node =>
        // Note that we pass the global variable scope, since other members of the struct should not be accessible in
        // default value expressions.
        // TODO: Once we introduce parametric declared types, we have to provide the struct's type scope.
        ExpressionTransformation.transform(node, member.tpe, registry.typeScope, registry.variableScope)
      }.toCompiledOption

      compiledDefaultValue.map(defaultValue => member.defaultValue = defaultValue)
    }.simultaneous.verification
  }

}
