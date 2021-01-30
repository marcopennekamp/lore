package lore.compiler.phases.transpilation

import lore.compiler.target.Target

class TemporaryVariableProvider(namePrefix: String = "") {
  private var counter = 0

  /**
    * Creates the next temporary variable.
    *
    * TODO: Needs to be thread-safe, probably.
    */
  def createVariable(): Target.Variable = {
    val variable = RuntimeNames.temporaryVariable(namePrefix + counter.toString)
    counter += 1
    variable
  }
}
