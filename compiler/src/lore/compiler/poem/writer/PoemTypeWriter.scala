package lore.compiler.poem.writer

import lore.compiler.core.CompilationException
import lore.compiler.poem.{PoemBasicType, PoemNamedType, PoemShapeType, PoemSymbolType, PoemType, PoemTypeVariable, PoemXaryType}
import lore.compiler.types.Kind

object PoemTypeWriter {

  private val tkMetadataKinded = 0
  private val tkSum = 1
  private val tkIntersection = 2
  private val tkTuple = 3
  private val tkNamed = 4

  private val mkAny = 0
  private val mkNothing = 1
  private val mkInt = 2
  private val mkReal = 3
  private val mkBoolean = 4
  private val mkString = 5
  private val mkVariable = 16
  private val mkFunction = 17
  private val mkList = 18
  private val mkMap = 19
  private val mkShape = 20
  private val mkSymbol = 21

  def write(tpe: PoemType)(implicit writer: BytecodeWriter): Unit = tpe match {
    case PoemTypeVariable(index) =>
      writeTypeTag(Kind.TypeVariable)
      writer.writeUInt8(index)

    case PoemBasicType(underlying) =>
      writeTypeTag(underlying.kind)

    case PoemXaryType(kind, types) =>
      writeTypeTag(kind, types.length)
      types.foreach(write)

    case tpe@PoemShapeType(properties) =>
      if (properties.size > 255) {
        throw CompilationException("Shape types cannot have more than 255 properties.")
      }

      writeTypeTag(Kind.Shape)
      writer.writeUInt8(properties.size)
      PoemWriter.writeShapePropertyNames(tpe.sortedNames, withCount = false)
      tpe.sortedProperties.foreach(write)

    case PoemSymbolType(name) =>
      writeTypeTag(Kind.Symbol)
      writer.writeStringWithLength(name)

    case PoemNamedType(schema, typeArguments) =>
      writeTypeTag(schema.kind, typeArguments.length)
      PoemWriter.writeNamePath(schema.name)
      typeArguments.foreach(write)
  }

  private def writeTypeTag(tagKind: Int, tagMetadata: Int)(implicit writer: BytecodeWriter): Unit = {
    val tag = ((tagKind & 0xFF) << 5) | (tagMetadata & 0xFF)
    writer.writeUInt8(tag)
  }

  private def writeTypeTag(kind: Kind, childCount: Int)(implicit writer: BytecodeWriter): Unit = {
    if (childCount > 31) {
      throw CompilationException(s"Types of kind $kind cannot have more than 31 type children.")
    }

    val (tagKind, tagMetadata) = kind match {
      case Kind.TypeVariable => (tkMetadataKinded, mkVariable)
      case Kind.Any => (tkMetadataKinded, mkAny)
      case Kind.Nothing => (tkMetadataKinded, mkNothing)
      case Kind.Int => (tkMetadataKinded, mkInt)
      case Kind.Real => (tkMetadataKinded, mkReal)
      case Kind.Boolean => (tkMetadataKinded, mkBoolean)
      case Kind.String => (tkMetadataKinded, mkString)
      case Kind.Sum => (tkSum, childCount)
      case Kind.Intersection => (tkIntersection, childCount)
      case Kind.Tuple => (tkTuple, childCount)
      case Kind.Function => (tkMetadataKinded, mkFunction)
      case Kind.List => (tkMetadataKinded, mkList)
      case Kind.Map => (tkMetadataKinded, mkMap)
      case Kind.Shape => (tkMetadataKinded, mkShape)
      case Kind.Symbol => (tkMetadataKinded, mkSymbol)
      case Kind.Trait => (tkNamed, childCount)
      case Kind.Struct => (tkNamed, childCount)
    }
    writeTypeTag(tagKind, tagMetadata)
  }

  private def writeTypeTag(kind: Kind)(implicit writer: BytecodeWriter): Unit = {
    writeTypeTag(kind, 0)
  }

}
