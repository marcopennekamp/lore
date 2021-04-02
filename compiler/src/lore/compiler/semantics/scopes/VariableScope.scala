package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Position
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to variables.
  */
trait VariableScope extends Scope[Variable] {
  def add(variable: Variable): Unit = add(variable.name, variable)
  def register(variable: Variable)(implicit position: Position): Verification = register(variable.name, variable)
}

/**
  * The root variable scope of a function.
  */
class FunctionVariableScope(val signature: FunctionSignature, parent: VariableScope) extends BasicScope[Variable](Some(parent)) with VariableScope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(this.add)
}

/**
  * A scope opened by a block.
  */
class BlockVariableScope(parent: VariableScope) extends BasicScope[Variable](Some(parent)) with VariableScope
