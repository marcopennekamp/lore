package lore.compiler

case class CompilerOptions(
  /**
   * Turns run-time logging on or off, which can greatly affect performance.
   */
  runtimeLogging: Boolean = true,
)
