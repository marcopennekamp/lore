package lore.compiler.poem

case class PoemFunction(
  name: String,
  typeParameters: Vector[PoemTypeParameter],
  inputType: PoemType,
  outputType: PoemType,
  isAbstract: Boolean,
  registerCount: Int,
  instructions: Vector[PoemInstruction],
)
