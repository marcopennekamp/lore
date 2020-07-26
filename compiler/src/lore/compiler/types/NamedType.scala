package lore.compiler.types

import lore.compiler.semantics.Scope

trait NamedType extends Type with Scope.Entry
