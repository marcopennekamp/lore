package lore.compiler.phases.transformation

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.{BlockBindingScope, BindingScope}

/**
  * A local variable scope context for expression transformation passes.
  */
class ScopeContext(parentScope: BindingScope) {
  private var scopes: List[BindingScope] = List(parentScope)

  def currentScope: BindingScope = scopes.head

  def openScope(): Unit = {
    val scope = new BlockBindingScope(currentScope)
    scopes = scope :: scopes
  }

  def closeScope(): Unit = {
    if (scopes.isEmpty) {
      throw CompilationException("The parent scope cannot not be closed from within the expression that's being transformed.")
    }
    scopes = scopes.tail
  }
}
