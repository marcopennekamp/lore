package lore.compiler.poem.writer

import lore.compiler.poem.{PoemFragment, PoemFunctionInstance, PoemMetaShape}

object PoemWriter {

  def writeFragment(poemFragment: PoemFragment): Array[Byte] = {
    implicit val writer: BytecodeWriter = new BytecodeWriter

    writer.writeString("poem")
    writer.writeUInt32(poemFragment.schemas.length)
    poemFragment.schemas.foreach(PoemSchemaWriter.write)
    writer.writeUInt32(poemFragment.globalVariables.length)
    poemFragment.globalVariables.foreach(PoemGlobalVariableWriter.write)
    writer.writeUInt32(poemFragment.functions.length)
    poemFragment.functions.foreach(PoemFunctionWriter.write)
    writer.writeUInt32(poemFragment.specs.length)
    poemFragment.specs.foreach(PoemSpecWriter.write)

    writer.output.toByteArray
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
