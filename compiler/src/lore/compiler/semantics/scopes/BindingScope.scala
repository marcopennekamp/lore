package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Position
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to bindings (variables, multi-functions, struct constructors, etc.).
  */
trait BindingScope extends Scope[Binding] {
  def add(binding: Binding): Unit = add(binding.name, binding)
  def register(binding: Binding, position: Position): Verification = register(binding.name, binding, position)

  override def entryLabel: String = "variable, multi-function, or struct constructor"
}

/**
  * The root binding scope of a function, containing parameter bindings.
  */
class FunctionBindingScope(val signature: FunctionSignature, parent: BindingScope) extends BasicScope[Binding](Some(parent)) with BindingScope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this operation
  // should not fail at this stage.
  signature.parameters.map(_.asVariable).foreach(this.add)

  override def entryLabel: String = "parameter"
}

/**
  * A scope opened by a block, containing local variable bindings.
  */
class BlockBindingScope(parent: BindingScope) extends BasicScope[Binding](Some(parent)) with BindingScope {
  override def entryLabel: String = "local variable"
}
