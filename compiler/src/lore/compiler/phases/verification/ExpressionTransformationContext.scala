package lore.compiler.phases.verification

import lore.compiler.semantics.{BlockScope, LocalVariable, Scope}

/**
  * A context for expression transformation passes, for example to hold local variable scopes.
  */
class ExpressionTransformationContext(parentScope: Scope[LocalVariable]) {
  private var scopes: List[Scope[LocalVariable]] = List(parentScope)

  def currentScope: Scope[LocalVariable] = scopes.head

  def openScope(): Unit = {
    val scope = new BlockScope(currentScope)
    scopes = scope :: scopes
  }

  def closeScope(): Unit = {
    assert(scopes.length > 1) // The parent scope should not be closed from within the expression, hence > 1.
    scopes = scopes.tail
  }
}
