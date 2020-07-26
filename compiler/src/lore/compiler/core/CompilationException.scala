package lore.compiler.core

// TODO: Report source positions!!!

/**
 * Signifies an unexpected compilation failure that is most likely a compiler bug. This type of error is clearly
 * differentiated from [[Feedback]] in that Feedback signifies a user error, while this
 * type of exception signifies a compiler error.
 */
case class CompilationException(message: String) extends RuntimeException(message + " This is a compiler bug!")
