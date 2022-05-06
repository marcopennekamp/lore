package lore.compiler.poem.writer

import lore.compiler.poem._
import lore.compiler.types.BasicType

object PoemValueWriter {

  def write(value: PoemValue)(implicit writer: BytecodeWriter): Unit = value match {
    case PoemIntValue(value) =>
      PoemTypeWriter.write(PoemBasicType(BasicType.Int))
      writer.writeInt64(value)

    case PoemRealValue(value) =>
      PoemTypeWriter.write(PoemBasicType(BasicType.Real))
      writer.writeFloat64(value)

    case PoemBooleanValue(value) =>
      PoemTypeWriter.write(PoemBasicType(BasicType.Boolean))
      writer.writeBoolean8(value)

    case PoemStringValue(value) =>
      PoemTypeWriter.write(PoemBasicType(BasicType.String))
      writer.writeStringWithLength(value)

    case PoemSymbolValue(name) =>
      PoemTypeWriter.write(PoemSymbolType(name))

    case PoemTupleValue(elements, tpe) =>
      PoemTypeWriter.write(tpe)
      elements.foreach(write)

    case value: PoemFunctionValue =>
      writeFunctionValueCommons(value)
      value match {
        case PoemSingleFunctionValue(_, typeArguments, _) => writer.writeManyWithCount8(typeArguments, PoemTypeWriter.write)
        case PoemFixedFunctionValue(_, inputType, _) => PoemTypeWriter.write(inputType)
        case _ =>
      }

    case PoemListValue(elements, tpe) =>
      PoemTypeWriter.write(tpe)
      writer.writeManyWithCount16(elements, write)

    case value@PoemShapeValue(_, tpe) =>
      PoemTypeWriter.write(tpe)
      value.sortedProperties.foreach(write)

    case value@PoemStructValue(_, tpe) =>
      PoemTypeWriter.write(tpe)
      writer.writeManyWithCount16(value.sortedProperties, write)
  }

  private def writeFunctionValueCommons(value: PoemFunctionValue)(implicit writer: BytecodeWriter): Unit = {
    PoemTypeWriter.write(value.tpe)
    writer.writeUInt8(value.variant.id)
    writer.writeNamePath(value.mf)
  }

}
