package lore.compiler.poem.writer

import lore.compiler.poem.PoemSpec

object PoemSpecWriter {

  def write(spec: PoemSpec)(implicit writer: BytecodeWriter): Unit = {
    writer.writeNamePath(spec.name)
    writer.writeBoolean8(spec.isTest)
    writer.writeBoolean8(spec.isBenchmark)
    writer.writeNamePath(spec.executableName)
  }

}
