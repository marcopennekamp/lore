package lore.compiler.types

import lore.types.Type

class TypeVariable(name: String, lowerBound: Type, upperBound: Type) extends lore.types.TypeVariable(name, lowerBound, upperBound) with lore.core.Scope.Entry
