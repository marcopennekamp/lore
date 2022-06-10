package lore.compiler.transformation

import lore.compiler.core.CompilationException
import lore.compiler.semantics.scopes.{TermScope, BlockTermScope}

/**
  * A local variable scope context for expression transformation passes.
  */
class ScopeContext(parentScope: TermScope) {
  private var scopes = Vector(new BlockTermScope(parentScope))

  def currentScope: BlockTermScope = scopes.last

  def openScope(): Unit = {
    val scope = new BlockTermScope(currentScope)
    scopes = scopes :+ scope
  }

  def closeScope(): Unit = {
    if (scopes.isEmpty) {
      throw CompilationException("The parent scope cannot be closed from within the expression that's being transformed.")
    }
    scopes = scopes.init
  }
}
