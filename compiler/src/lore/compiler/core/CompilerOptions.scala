package lore.compiler.core

case class CompilerOptions(
  /**
    * Report errors and warnings with stack traces.
    */
  showFeedbackStackTraces: Boolean = false,
)
