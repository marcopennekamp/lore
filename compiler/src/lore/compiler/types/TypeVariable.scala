package lore.compiler.types

import lore.types.Type

class TypeVariable(
  name: String,
  lowerBound: Type,
  upperBound: Type,
  val declarationOrder: Int
) extends lore.types.TypeVariable(name, lowerBound, upperBound) with lore.core.Scope.Entry
