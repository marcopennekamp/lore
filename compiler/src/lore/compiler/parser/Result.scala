package lore.compiler.parser

/**
  * The result of a parsing run. A [[Failure]] signifies that a parser encountered at least one error. By convention,
  * a failure result should be accompanied by at least one reported error (reported via [[Parser.reporter]]). The
  * `backtrack` functions will backtrack the reported errors of a [[Failure]], but it should be used sparingly.
  *
  * To improve error reporting, errors reported by a specific parser should be conserved if at all possible, as the most
  * specific parser can usually report the best error. Once a parse run has been backtracked, all errors it produced
  * will also be backtracked, leading to a situation where the more general parser needs to formulate its own error, but
  * with less information. In the worst case, the top-level parser will just report the next token and its position.
  *
  * To make this possible, parsers should try to use lookahead (via [[Parser.peek]]) to avoid backtracking. For example,
  * an annotation list parser can invoke the annotation parser without having to backtrack if it establishes that the
  * next token is a [[lore.compiler.syntax.TkAnnotation]].
  *
  * Another example, an object type alias parser is recoverable until the `=` has been encountered. `object Name` can
  * be parsed by the alternative object declaration parser, but `object Name =` cannot. So the module member declaration
  * parser can look ahead whether a `=` or `[` follows after the object's name, which proves it to be a type alias.
  */
sealed trait Result[+A] {
  def isSuccess: Boolean
  def isRecoverable: Boolean

  def getOrElse[B >: A](alternative: => B): B
  def map[B](f: A => B): Result[B]
}

case class Success[+A](value: A) extends Result[A] {
  override def isSuccess: Boolean = true
  override def isRecoverable: Boolean = false

  override def getOrElse[B >: A](alternative: => B): B = value
  override def map[B](f: A => B): Result[B] = Success(f(value))
}

object Success {
  val empty: Result[Unit] = Success(())
}

case object Failure extends Result[Nothing] {
  override def isSuccess: Boolean = false
  override def isRecoverable: Boolean = true

  override def getOrElse[B >: Nothing](alternative: => B): B = alternative
  override def map[B](f: Nothing => B): Result[B] = Failure
}
