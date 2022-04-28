package lore.compiler.poem.writer

import lore.compiler.poem.PoemFunction

object PoemFunctionWriter {

  def write(function: PoemFunction)(implicit writer: BytecodeWriter): Unit = {
    writer.writeNamePath(function.name)
    PoemTypeParameterWriter.write(function.typeParameters)
    PoemTypeWriter.write(function.inputType)
    PoemTypeWriter.write(function.outputType)
    writer.writeBoolean8(function.isAbstract)

    if (!function.isAbstract) {
      // Writing instructions fills the constants table, so we have to write the instructions first, then later concat
      // them to the parent writer.
      implicit val constantsTable: ConstantsTable = new ConstantsTable
      val instructionStream = BytecodeWriter.nested { implicit writer =>
        writer.writeManyWithCount16(function.instructions, PoemInstructionWriter.write)
      }
      ConstantsTableWriter.write(constantsTable)
      writer.writeUInt16(function.registerCount)
      writer.writeStream(instructionStream)
    }
  }

}
