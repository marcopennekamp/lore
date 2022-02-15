package lore.compiler.poem.writer

import lore.compiler.poem.{PoemBasicType, PoemBooleanValue, PoemFixedFunctionValue, PoemFunctionValue, PoemIntValue, PoemLambdaFunctionValue, PoemListValue, PoemMultiFunctionValue, PoemRealValue, PoemShapeValue, PoemStringValue, PoemStructValue, PoemSymbolType, PoemSymbolValue, PoemTupleValue, PoemValue}
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
      writer.writeBoolean(value)

    case PoemStringValue(value) =>
      PoemTypeWriter.write(PoemBasicType(BasicType.String))
      writer.writeStringWithLength(value)

    case PoemTupleValue(elements, tpe) =>
      PoemTypeWriter.write(tpe)
      elements.foreach(write)

    case value: PoemFunctionValue =>
      value match {
        case PoemFixedFunctionValue(mf, inputType, tpe) => ???
        case PoemLambdaFunctionValue(mf, tpe) => ???
        case PoemMultiFunctionValue(mf, tpe) => ???
      }

    case PoemListValue(elements, tpe) =>
      PoemTypeWriter.write(tpe)
      PoemWriter.writeManyWithCount16(elements, write)

    case value@PoemShapeValue(_, tpe) =>
      PoemTypeWriter.write(tpe)
      value.sortedProperties.foreach(write)

    case PoemSymbolValue(name) =>
      PoemTypeWriter.write(PoemSymbolType(name))

    case value@PoemStructValue(_, tpe) =>
      PoemTypeWriter.write(tpe)
      PoemWriter.writeManyWithCount16(value.sortedProperties, write)
  }

}
