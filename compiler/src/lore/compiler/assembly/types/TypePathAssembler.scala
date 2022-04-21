package lore.compiler.assembly.types

import lore.compiler.assembly.{Chunk, RegisterProvider}
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.types.TypePath

object TypePathAssembler {

  /**
    * Generates instructions that apply the given type path to `origin`, which must be a register holding a
    * <i>type</i>. The result of these instructions will be the subterm of the type that was sought through the type
    * path.
    */
  def generate(origin: Poem.Register, typePath: TypePath)(implicit registerProvider: RegisterProvider): Chunk = {
    val target = registerProvider.fresh()
    typePath.steps.foldLeft(Chunk(origin)) {
      case (chunk, step) =>
        val source = chunk.forceResult
        val instruction = step match {
          case TypePath.TupleElement(index) => PoemInstruction.TypePathIndex(target, source, index)
          case TypePath.FunctionInput => PoemInstruction.TypePathIndex(target, source, 0)
          case TypePath.FunctionOutput => PoemInstruction.TypePathIndex(target, source, 1)
          case TypePath.ListElement => PoemInstruction.TypePathIndex(target, source, 0)
          case TypePath.MapKey => PoemInstruction.TypePathIndex(target, source, 0)
          case TypePath.MapValue => PoemInstruction.TypePathIndex(target, source, 1)
          case TypePath.ShapeProperty(name) => PoemInstruction.TypePathProperty(target, source, name)
          case TypePath.TypeArgument(schema, index) => PoemInstruction.TypePathTypeArgument(target, source, schema, index)
        }
        chunk ++ Chunk(target, instruction)
    }
  }

}
