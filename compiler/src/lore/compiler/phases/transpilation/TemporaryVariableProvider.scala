package lore.compiler.phases.transpilation

import lore.compiler.target.Target

import java.util.concurrent.atomic.AtomicInteger

class TemporaryVariableProvider(namePrefix: String = "") {
  private val nameCounter: AtomicInteger = new AtomicInteger()

  /**
    * Creates the next temporary variable.
    */
  def createVariable(): Target.Variable = RuntimeNames.temporaryVariable(namePrefix + nameCounter.incrementAndGet().toString)
}
