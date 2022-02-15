package lore.compiler.poem

import lore.compiler.types.TypeVariable.Variance

case class PoemTypeParameter(
  name: String,
  lowerBound: PoemType,
  upperBound: PoemType,
  variance: Variance,
)
