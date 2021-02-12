package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Position
import lore.compiler.semantics.LocalVariable
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to variables.
  */
trait VariableScope extends Scope[LocalVariable] {
  def add(variable: LocalVariable): Unit = add(variable.name, variable)
  def register(variable: LocalVariable)(implicit position: Position): Verification = register(variable.name, variable)
}

/**
  * The global variable scope does not have any entries yet, since there are no global variables (and functions
  * aren't counted among such names yet), but this declaration will hopefully make the code a little more future-proof.
  */
class GlobalVariableScope() extends BasicScope[LocalVariable](None) with VariableScope {
  // The global scope is empty as of now.
}

/**
  * The root variable scope of a function.
  */
class FunctionVariableScope(val signature: FunctionSignature, parent: VariableScope) extends BasicScope[LocalVariable](Some(parent)) with VariableScope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(this.add)
}

/**
  * A scope opened by a block.
  */
class BlockVariableScope(parent: VariableScope) extends BasicScope[LocalVariable](Some(parent)) with VariableScope
