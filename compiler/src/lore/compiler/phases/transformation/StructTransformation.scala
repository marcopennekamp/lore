package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.Verification
import lore.compiler.phases.transpilation.TranspiledName
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.DynamicCallTarget
import lore.compiler.semantics.structures.{DefaultValue, StructDefinition}

object StructTransformation {

  /**
    * Transforms the default value expressions of struct members and assigns them to their 'defaultValue' fields.
    */
  def transform(struct: StructDefinition)(implicit registry: Registry): Verification = {
    struct.members.map { member =>
      val compiledExpression = member.defaultValueNode.map { node =>
        // Note that we pass the global variable scope, since other members of the struct should not be accessible in
        // default value expressions.
        // TODO: Once we introduce parametric declared types, we have to provide the struct's type scope.
        ExpressionTransformation.transform(node, member.tpe, registry.typeScope, registry.variableScope)
      }.toCompiledOption

      compiledExpression.map { maybeExpression =>
        member.defaultValue = maybeExpression.map { expression =>
          DefaultValue(expression, DynamicCallTarget(TranspiledName.defaultValue(struct.tpe, member).name, expression.tpe))
        }
      }
    }.simultaneous.verification
  }

}
