package lore.compiler.phases.verification

import lore.compiler.Compilation
import lore.compiler.Compilation.{C, Verification}
import lore.compiler.feedback.{Error, Position}
import lore.compiler.phases.verification.Scope.{UnknownVariable, VariableAlreadyDeclared}
import lore.compiler.definitions.FunctionSignature
import lore.types.Type

import scala.collection.mutable

abstract class Scope {
  protected val variables: mutable.Map[String, LocalVariable] = new mutable.HashMap()

  /**
    * Fetches the variable with the given name from the closest scope.
    */
  def get(name: String): Option[LocalVariable] = variables.get(name)

  /**
    * Adds the given variable to the scope.
    */
  protected def add(variable: LocalVariable): Unit = {
    assert(!variables.contains(variable.name))
    variables.put(variable.name, variable)
  }

  /**
    * Fetches the variable with the given name from the closest scope. If it cannot be found, we return a
    * compilation error.
    */
  def variable(name: String, position: Position): C[LocalVariable] = {
     get(name) match {
      case Some(variable) => Compilation.succeed(variable)
      case None => Compilation.fail(UnknownVariable(name, position))
    }
  }

  /**
    * Registers the given variable with the scope. If it is already registered in the CURRENT scope, a verification
    * error is returned instead.
    */
  def register(variable: LocalVariable, position: Position): Verification = {
    if (variables.contains(variable.name)) {
      Compilation.fail(VariableAlreadyDeclared(variable.name, position))
    } else {
      add(variable)
      Verification.succeed
    }
  }
}

object Scope {
  case class VariableAlreadyDeclared(name: String, pos: Position) extends Error(pos) {
    override def message = s"A variable $name has already been declared in the current scope."
  }

  case class UnknownVariable(name: String, pos: Position) extends Error(pos) {
    override def message = s"The current scope does not know a variable $name."
  }
}

class FunctionScope(val signature: FunctionSignature) extends Scope {
  // Register all parameters as immutable variables with the function scope. We bypass 'register' since this
  // operation should not fail at this stage.
  signature.parameters.map(_.asLocalVariable).foreach(this.add)
}

class BlockScope(val parent: Scope) extends Scope {
  override def get(name: String): Option[LocalVariable] = super.get(name).orElse(parent.get(name))
}

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean)
