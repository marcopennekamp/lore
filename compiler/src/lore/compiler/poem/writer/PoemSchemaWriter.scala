package lore.compiler.poem.writer

import lore.compiler.core.CompilationException
import lore.compiler.poem.{PoemSchema, PoemStructProperty, PoemStructSchema, PoemTraitSchema}
import lore.compiler.types.Kind

object PoemSchemaWriter {

  def write(schema: PoemSchema)(implicit writer: BytecodeWriter): Unit = {
    val kindCode = schema.kind match {
      case Kind.Trait => 0
      case Kind.Struct => 1
      case _ => throw CompilationException(s"Invalid kind for PoemSchema ${schema.name}: ${schema.kind}.")
    }
    writer.writeUInt8(kindCode)
    writer.writeNamePath(schema.name)
    PoemTypeParameterWriter.write(schema.typeParameters)
    writer.writeManyWithCount8(schema.supertraits, PoemTypeWriter.write)

    schema match {
      case schema: PoemTraitSchema =>
        PoemTypeWriter.write(schema.inheritedShapeType)

      case schema: PoemStructSchema =>
        writer.writeManyWithCount16(schema.properties, writeStructProperty)
    }
  }

  private def writeStructProperty(property: PoemStructProperty)(implicit writer: BytecodeWriter): Unit = {
    writer.writeStringWithLength(property.name)
    PoemTypeWriter.write(property.tpe)
    writer.writeBoolean8(property.isOpen)
  }

}
