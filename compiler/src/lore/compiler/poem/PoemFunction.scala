package lore.compiler.poem

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature

case class PoemFunction(
  signature: FunctionSignature,
  typeParameters: Vector[PoemTypeParameter],
  inputType: PoemType,
  outputType: PoemType,
  isAbstract: Boolean,
  registerCount: Int,
  instructions: Vector[PoemInstruction],
) {
  val name: NamePath = signature.name
}
