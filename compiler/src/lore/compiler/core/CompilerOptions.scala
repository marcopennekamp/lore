package lore.compiler.core

case class CompilerOptions(
  /**
    * Run-time logging generates various logging statements in the target Javascript code, which can greatly affect performance.
    */
  enableRuntimeLogging: Boolean = false,

  /**
    * Report errors and warnings with stack traces.
    */
  showFeedbackStackTraces: Boolean = false,
)

object CompilerOptions {
  val defaultBaseDirectory = "."
  val defaultOutputFileName = "lore-program.js"
}
