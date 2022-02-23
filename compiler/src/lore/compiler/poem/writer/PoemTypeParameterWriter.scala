package lore.compiler.poem.writer

import lore.compiler.core.CompilationException
import lore.compiler.poem.PoemTypeParameter
import lore.compiler.types.TypeVariable.Variance

object PoemTypeParameterWriter {

  /**
    * The VM supports a maximum of 32 type parameters.
    */
  val maximumTypeParameters = 32

  def write(typeParameters: Vector[PoemTypeParameter])(implicit writer: BytecodeWriter): Unit = {
    if (typeParameters.length > maximumTypeParameters) {
      throw CompilationException(s"A schema or function has ${typeParameters.length} type parameters, but the maximum is $maximumTypeParameters.")
    }
    writer.writeManyWithCount8(typeParameters, writeTypeParameter)
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

}
