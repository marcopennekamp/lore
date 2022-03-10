package lore.compiler.poem.writer

import lore.compiler.poem.PoemFunction

object PoemFunctionWriter {

  def write(function: PoemFunction)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeNamePath(function.name)
    PoemTypeParameterWriter.write(function.typeParameters)
    PoemTypeWriter.write(function.inputType)
    PoemTypeWriter.write(function.outputType)
    writer.writeBoolean8(function.isAbstract)

    if (!function.isAbstract) {
      writer.writeUInt16(function.registerCount)
      writer.writeManyWithCount16(function.instructions, PoemInstructionWriter.write)
    }
  }

}
