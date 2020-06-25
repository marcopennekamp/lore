package lore.compiler.phases.verification

import lore.compiler.core.{BasicScope, Scope}
import lore.compiler.functions.FunctionSignature
import lore.types.Type

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean) extends lore.core.Scope.Entry

class FunctionScope(val signature: FunctionSignature) extends BasicScope[LocalVariable](None) {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(this.add)
}

class BlockScope(parent: Scope[LocalVariable]) extends BasicScope[LocalVariable](Some(parent)) {
  override def get(name: String): Option[LocalVariable] = super.get(name).orElse(parent.get(name))
}
