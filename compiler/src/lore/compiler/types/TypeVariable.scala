package lore.compiler.types

import lore.compiler.core.Scope
import lore.types.Type

class TypeVariable(name: String, bound: Type) extends lore.types.TypeVariable(name, bound) with Scope.Entry
