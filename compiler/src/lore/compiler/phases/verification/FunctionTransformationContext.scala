package lore.compiler.phases.verification

import lore.compiler.semantics.{BlockScope, FunctionScope, LocalVariable, Scope}
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A context for function transformation passes, for example to hold local variable scopes.
  */
class FunctionTransformationContext(signature: FunctionSignature) {
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
