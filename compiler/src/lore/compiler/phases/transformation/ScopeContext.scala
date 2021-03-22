package lore.compiler.phases.transformation

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.{BlockVariableScope, VariableScope}

/**
  * A local variable scope context for expression transformation passes.
  */
class ScopeContext(parentScope: VariableScope) {
  private var scopes: List[VariableScope] = List(parentScope)

  def currentScope: VariableScope = scopes.head

  def openScope(): Unit = {
    val scope = new BlockVariableScope(currentScope)
    scopes = scope :: scopes
  }

  def closeScope(): Unit = {
    if (scopes.isEmpty) {
      throw CompilationException("The parent scope cannot not be closed from within the expression that's being transformed.")
    }
    scopes = scopes.tail
  }
}
