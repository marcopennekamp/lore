package lore.compiler.phases.transpilation

class TemporaryNameProvider(prefix: String = "") {
  private var counter = 0

  /**
    * Creates the next temporary name.
    */
  def createName(): String = {
    val name = s"$prefix${TranspiledNames.temporaryVariable(counter.toString)}"
    counter += 1
    name
  }
}
