package lore.compiler.core

case class CompilerOptions(
  /**
    * Run-time logging generates various logging statements in the target Javascript code, which can greatly affect performance.
    *
    * TODO (assembly): Remove this option, as it has no place with the VM as a runtime.
    */
  enableRuntimeLogging: Boolean = false,

  /**
    * Report errors and warnings with stack traces.
    */
  showFeedbackStackTraces: Boolean = false,
)
