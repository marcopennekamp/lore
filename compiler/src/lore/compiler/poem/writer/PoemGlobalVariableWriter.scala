package lore.compiler.poem.writer

import lore.compiler.poem.{PoemEagerGlobalVariable, PoemGlobalVariable, PoemLazyGlobalVariable}

object PoemGlobalVariableWriter {

  def write(variable: PoemGlobalVariable)(implicit writer: BytecodeWriter): Unit = {
    writer.writeNamePath(variable.name)
    variable match {
      case variable: PoemEagerGlobalVariable =>
        writer.writeBoolean(false) // Lazy = false
        PoemValueWriter.write(variable.value)

      case variable: PoemLazyGlobalVariable =>
        writer.writeBoolean(true) // Lazy = true
        writer.writeNamePath(variable.initializerName)
    }
  }

}
