package lore.compiler.phases.transpilation

import lore.compiler.target.Target.TargetName
import lore.compiler.target.TargetDsl.StringExtension

// TODO: Provide a variable right away. Rename to TemporaryVariableProvider.

class TemporaryNameProvider(prefix: String = "") {
  private var counter = 0

  /**
    * Creates the next temporary name.
    *
    * TODO: Needs to be thread-safe, probably.
    */
  def createName(): TargetName = {
    val name = s"$prefix${RuntimeNames.temporaryVariable(counter.toString)}".asName
    counter += 1
    name
  }
}
