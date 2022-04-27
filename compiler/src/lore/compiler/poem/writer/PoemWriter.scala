package lore.compiler.poem.writer

import lore.compiler.poem.{PoemFragment, PoemFunctionInstance, PoemMetaShape}

object PoemWriter {

  def writeFragment(poemFragment: PoemFragment): Array[Byte] = {
    implicit val constantsTable: ConstantsTable = new ConstantsTable

    // Writing poem functions fills the constants table, so we have to write all schemas, global variables, and
    // functions first, then later concat them to the full output writer.
    val entriesWriter = writeEntries(poemFragment)
    implicit val writer: BytecodeWriter = new BytecodeWriter()
    writer.writeString("poem")
    ConstantsTableWriter.write(constantsTable)

    // This copies all bytes written to `entriesWriter` to the main writer.
    entriesWriter.output.writeTo(writer.output)

    writer.output.toByteArray
  }

  private def writeEntries(poemFragment: PoemFragment)(implicit constantsTable: ConstantsTable): BytecodeWriter = {
    implicit val writer: BytecodeWriter = new BytecodeWriter()

    // No schemas and global variables yet.
    writer.writeUInt16(poemFragment.schemas.length)
    poemFragment.schemas.foreach(PoemSchemaWriter.write)

    writer.writeUInt16(poemFragment.globalVariables.length)
    poemFragment.globalVariables.foreach(PoemGlobalVariableWriter.write)

    writer.writeUInt16(poemFragment.functions.length)
    poemFragment.functions.foreach(PoemFunctionWriter.write)

    writer
  }

  def writeFunctionInstance(instance: PoemFunctionInstance)(implicit writer: BytecodeWriter): Unit = {
    writer.writeNamePath(instance.name)
    writer.writeManyWithCount8(instance.typeArguments, PoemTypeWriter.write)
  }

  def writeMetaShape(metaShape: PoemMetaShape)(implicit writer: BytecodeWriter): Unit = {
    writeShapePropertyNames(metaShape.names, withCount = true)
  }

  def writeShapePropertyNames(names: Vector[String], withCount: Boolean)(implicit writer: BytecodeWriter): Unit = {
    if (withCount) writer.writeManyWithCount16(names, writer.writeStringWithLength)
    else names.foreach(writer.writeStringWithLength)
  }

}
