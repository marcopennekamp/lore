package lore.compiler.poem

import lore.compiler.semantics.NamePath

case class PoemFunction(
  name: NamePath,
  typeParameters: Vector[PoemTypeParameter],
  inputType: PoemType,
  outputType: PoemType,
  isAbstract: Boolean,
  registerCount: Int,
  instructions: Vector[PoemInstruction],
)
