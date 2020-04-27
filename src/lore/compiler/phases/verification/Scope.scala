package lore.compiler.phases.verification

import lore.compiler.Compilation
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.{Error, Position}
import lore.compiler.phases.verification.Scope.VariableAlreadyDeclared
import lore.definitions.FunctionSignature
import lore.types.Type

import scala.collection.mutable

abstract class Scope {
  protected val variables: mutable.Map[String, LocalVariable] = new mutable.HashMap()

  /**
    * Fetches the variable with the given name from the closest scope.
    */
  def variable(name: String): Option[LocalVariable] = variables.get(name)

  /**
    * Registers the given variable with the scope. If it is already registered in the CURRENT scope, a verification
    * error is returned instead.
    */
  def register(variable: LocalVariable, position: Position): Verification = {
    if (variables.contains(variable.name)) {
      Compilation.fail(VariableAlreadyDeclared(variable.name, position))
    } else {
      variables.put(variable.name, variable)
      Verification.succeed
    }
  }
}

object Scope {
  case class VariableAlreadyDeclared(name: String, pos: Position) extends Error(pos) {
    override def message = s"A variable $name has already been declared in the current scope."
  }
}

class FunctionScope(val signature: FunctionSignature) extends Scope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(v => variables.put(v.name, v))
}

class BlockScope(val parent: Scope) extends Scope {
  override def variable(name: String): Option[LocalVariable] = super.variable(name).orElse(parent.variable(name))
}

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean)
