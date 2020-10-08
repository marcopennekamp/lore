package lore.compiler.phases.transpilation

import lore.compiler.phases.transpilation.TranspiledName.StringExtension

class TemporaryNameProvider(prefix: String = "") {
  private var counter = 0

  /**
    * Creates the next temporary name.
    */
  def createName(): TranspiledName = {
    val name = s"$prefix${TranspiledName.temporaryVariable(counter.toString)}".asName
    counter += 1
    name
  }
}
