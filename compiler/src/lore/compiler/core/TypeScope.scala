package lore.compiler.core

import lore.compiler.types.NamedType

trait TypeScope extends Scope[NamedType]

/**
  * A scope that provides access to declared type variables. The parent MUST be defined, either as another
  * type variable scope or the registry.
  */
class TypeVariableScope(parent: TypeScope) extends BasicScope[NamedType](Some(parent)) with TypeScope
