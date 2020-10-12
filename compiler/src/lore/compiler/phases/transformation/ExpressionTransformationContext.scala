package lore.compiler.phases.transformation

import lore.compiler.semantics.{BlockScope, LocalVariable, Scope, VariableScope}

/**
  * A context for expression transformation passes, for example to hold local variable scopes.
  */
class ExpressionTransformationContext(parentScope: VariableScope) {
  private var scopes: List[VariableScope] = List(parentScope)

  def currentScope: VariableScope = scopes.head

  def openScope(): Unit = {
    val scope = new BlockScope(currentScope)
    scopes = scope :: scopes
  }

  def closeScope(): Unit = {
    assert(scopes.length > 1) // The parent scope should not be closed from within the expression, hence > 1.
    scopes = scopes.tail
  }
}
