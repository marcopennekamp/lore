package lore.compiler.phases.transpilation

class TemporaryNameProvider {
  private var counter = 0

  /**
    * Creates the next temporary name.
    */
  def createName(): String = {
    val name = s"tmp$counter"
    counter += 1
    name
  }
}
