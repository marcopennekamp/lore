package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to bindings (variables, multi-functions, struct constructors, etc.).
  */
trait BindingScope extends Scope[Binding] {
  def add(binding: Binding): Unit = add(binding.name, binding)
  def register[A <: Binding](binding: A, position: Position)(implicit reporter: Reporter): Unit = register(binding.name, binding, position)

  override def entryLabel: String = "variable, multi-function, or struct constructor"
}

/**
  * The root binding scope of a function, containing parameter bindings.
  */
class FunctionBindingScope(val signature: FunctionSignature, parent: BindingScope) extends BasicScope[Binding](Some(parent)) with BindingScope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this operation
  // should not fail at this stage.
  signature.parameters.map(_.asVariable).foreach(this.add)
}

/**
  * A scope opened by a block, containing local variable bindings.
  */
class BlockBindingScope(parent: BindingScope) extends BasicScope[Binding](Some(parent)) with BindingScope
