package lore.compiler.semantics

import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to variables.
  */
trait VariableScope extends Scope[LocalVariable]

/**
  * The global variable scope does not have any entries yet, since there are no global variables (and functions
  * aren't counted among such names yet), but this will hopefully make the code a little more future-proof.
  */
class GlobalScope() extends BasicScope[LocalVariable](None) with VariableScope {
  // The global scope is empty as of now.
}

/**
  * The root scope of a function.
  */
class FunctionScope(val signature: FunctionSignature, parent: VariableScope) extends BasicScope[LocalVariable](Some(parent)) with VariableScope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(this.add)
}

/**
  * A scope opened by a block.
  */
class BlockScope(parent: VariableScope) extends BasicScope[LocalVariable](Some(parent)) with VariableScope {
  override def get(name: String): Option[LocalVariable] = super.get(name).orElse(parent.get(name))
}
