package lore.compiler.poem.writer

import lore.compiler.core.CompilationException
import lore.compiler.poem.{PoemFragment, PoemFunction, PoemMetaShape, PoemTypeParameter}
import lore.compiler.semantics.NamePath
import lore.compiler.types.TypeVariable.Variance

object PoemWriter {

  /**
    * The VM supports a maximum of 32 type parameters.
    */
  val maximumTypeParameters = 32

  def writeFragment(poemFragment: PoemFragment): Array[Byte] = {
    implicit val constantsTable: ConstantsTable = new ConstantsTable

    // Writing poem functions fills the constants table, so we have to write all schemas, global variables, and
    // functions first, then later concat them to the full output writer.
    val entriesWriter = writeEntries(poemFragment)
    implicit val writer: BytecodeWriter = new BytecodeWriter()
    writer.writeString("poem")
    writeConstantsTable(constantsTable)

    // This copies all bytes written to `entriesWriter` to the main writer.
    entriesWriter.output.writeTo(writer.output)

    writer.output.toByteArray
  }

  private def writeEntries(poemFragment: PoemFragment)(implicit constantsTable: ConstantsTable): BytecodeWriter = {
    implicit val writer: BytecodeWriter = new BytecodeWriter()

    // No schemas and global variables yet.
    writer.writeUInt16(0)
    writer.writeUInt16(0)

    writer.writeUInt16(poemFragment.functions.length)
    poemFragment.functions.foreach(writeFunction)

    writer
  }

  private def writeConstantsTable(constantsTable: ConstantsTable)(implicit writer: BytecodeWriter): Unit = {
    writeManyWithCount16(constantsTable.types, PoemTypeWriter.write)
    writeManyWithCount16(constantsTable.values, PoemValueWriter.write)
    writeManyWithCount16(constantsTable.names, writer.writeStringWithLength)
    writeManyWithCount16(constantsTable.intrinsics.map(_.name), writer.writeStringWithLength)
    writeManyWithCount16(constantsTable.schemas, writeNamePath)
    writeManyWithCount16(constantsTable.globalVariables, writeNamePath)
    writeManyWithCount16(constantsTable.multiFunctions, writeNamePath)
    writeManyWithCount16(constantsTable.metaShapes, writeMetaShape)
  }

  private def writeFunction(function: PoemFunction)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeStringWithLength(function.name)
    writeTypeParameters(function.typeParameters)
    PoemTypeWriter.write(function.inputType)
    PoemTypeWriter.write(function.outputType)
    writer.writeBoolean(function.isAbstract)

    if (!function.isAbstract) {
      writer.writeUInt16(function.registerCount)
      writeManyWithCount16(function.instructions, PoemInstructionWriter.write)
    }
  }

  private def writeMetaShape(metaShape: PoemMetaShape)(implicit writer: BytecodeWriter): Unit = {
    writeShapePropertyNames(metaShape.names, withCount = true)
  }

  def writeShapePropertyNames(names: Vector[String], withCount: Boolean)(implicit writer: BytecodeWriter): Unit = {
    if (withCount) writeManyWithCount16(names, writer.writeStringWithLength)
    else names.foreach(writer.writeStringWithLength)
  }

  private def writeTypeParameters(typeParameters: Vector[PoemTypeParameter])(implicit writer: BytecodeWriter): Unit = {
    if (typeParameters.length > maximumTypeParameters) {
      throw CompilationException(s"A schema or function has ${typeParameters.length} type parameters, but the maximum is $maximumTypeParameters.")
    }
    writeManyWithCount8(typeParameters, writeTypeParameter)
  }

  private def writeTypeParameter(typeParameter: PoemTypeParameter)(implicit writer: BytecodeWriter): Unit = {
    writer.writeStringWithLength(typeParameter.name)
    PoemTypeWriter.write(typeParameter.lowerBound)
    PoemTypeWriter.write(typeParameter.upperBound)

    val varianceId = typeParameter.variance match {
      case Variance.Covariant => 0
      case Variance.Contravariant => 1
      case Variance.Invariant => 2
    }
    writer.writeUInt8(varianceId)
  }

  def writeNamePath(namePath: NamePath)(implicit writer: BytecodeWriter): Unit = {
    writer.writeStringWithLength(namePath.toString)
  }

  def writeManyWithCount8[A](values: Vector[A], write: A => Unit)(implicit writer: BytecodeWriter): Unit = {
    writer.writeUInt8(values.length)
    values.foreach(write)
  }

  def writeManyWithCount16[A](values: Vector[A], write: A => Unit)(implicit writer: BytecodeWriter): Unit = {
    writer.writeUInt16(values.length)
    values.foreach(write)
  }

}
