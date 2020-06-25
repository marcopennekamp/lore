package lore.compiler.phases.verification

import lore.compiler.core.Scope
import lore.compiler.functions.FunctionSignature

/**
  * A context for function verification passes, for example to hold local variable scopes.
  */
class FunctionVerificationContext(signature: FunctionSignature) {
  private var scopes: List[Scope[LocalVariable]] = List(new FunctionScope(signature))
  def currentScope: Scope[LocalVariable] = scopes.head
  def openScope(): Unit = {
    val scope = new BlockScope(currentScope)
    scopes = scope :: scopes
  }
  def closeScope(): Unit = {
    assert(scopes.length > 1) // The function scope should not be closed, hence > 1.
    scopes = scopes.tail
  }
}
