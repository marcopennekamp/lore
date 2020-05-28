package lore.compiler.phases

import lore.compiler.core.Compilation.C

trait Phase[Result] {
  /**
    * The compilation result of this phase.
    */
  def result: C[Result]
}
