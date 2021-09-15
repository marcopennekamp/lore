package lore.compiler.transformation

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.{BindingScope, BlockBindingScope}

/**
  * A local variable scope context for expression transformation passes.
  */
class ScopeContext(parentScope: BindingScope) {
  private var scopes = Vector(new BlockBindingScope(parentScope))

  def currentScope: BlockBindingScope = scopes.last

  def openScope(): Unit = {
    val scope = new BlockBindingScope(currentScope)
    scopes = scopes :+ scope
  }

  def closeScope(): Unit = {
    if (scopes.isEmpty) {
      throw CompilationException("The parent scope cannot be closed from within the expression that's being transformed.")
    }
    scopes = scopes.init
  }
}
