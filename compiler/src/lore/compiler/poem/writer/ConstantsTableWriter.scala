package lore.compiler.poem.writer

object ConstantsTableWriter {

  def write(constantsTable: ConstantsTable)(implicit writer: BytecodeWriter): Unit = {
    writer.writeManyWithCount16(constantsTable.entries, writeEntry)
  }

  private def writeEntry(entry: ConstantsTableEntry)(implicit writer: BytecodeWriter): Unit = {
    writer.writeUInt8(entry.variant.id)
    entry match {
      case ConstantsTableEntry.TypeEntry(tpe) => PoemTypeWriter.write(tpe)
      case ConstantsTableEntry.ValueEntry(value) => PoemValueWriter.write(value)
      case ConstantsTableEntry.NameEntry(name) => writer.writeStringWithLength(name)
      case ConstantsTableEntry.IntrinsicEntry(intrinsic) => writer.writeStringWithLength(intrinsic.name)
      case ConstantsTableEntry.SchemaEntry(name) => writer.writeNamePath(name)
      case ConstantsTableEntry.GlobalVariableEntry(name) => writer.writeNamePath(name)
      case ConstantsTableEntry.MultiFunctionEntry(name) => writer.writeNamePath(name)
      case ConstantsTableEntry.FunctionInstanceEntry(instance) => PoemWriter.writeFunctionInstance(instance)
      case ConstantsTableEntry.MetaShapeEntry(metaShape) => PoemWriter.writeMetaShape(metaShape)
    }
  }

}
