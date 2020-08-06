package lore.compiler.core

trait Phase[Result] {
  /**
    * The compilation result of this phase.
    */
  def result: Compilation[Result]
}
