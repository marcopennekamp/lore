package lore.compiler.parser

/**
  * The result of a parsing run. We support two modes of failure to improve the granularity of backtracking. If a
  * parser is in a state from which no amount of backtracking would recover the run, [[Failure]] should be returned.
  * Otherwise, if the run can feasibly be recovered, [[Recoverable]] should be returned. `backtrack` functions only
  * backtrack from [[Recoverable]], not [[Failure]].
  *
  * Two modes of failure improve error reporting, as the most specific parser can usually report the best error. Once
  * a parse run has been backtracked, all errors it produced will also be backtracked, leading to a situation where
  * the more general parser needs to formulate its own error, but with less information.
  *
  * For example, a `@where` annotation parser can be recovered from if it hasn't yet parsed the `@where`. But if the
  * parser failed to parse the type parameters, or the annotation wasn't correctly terminated, such failures cannot be
  * recovered from, because there is no other parser which might parse the `@where` differently. Hence, the very
  * specific errors produced by the `@where` parser should be shown to the user, instead of trying to backtrack.
  *
  * Another example, an object type alias parser is recoverable until the `=` has been encountered. `object Name` can
  * be parsed by the alternative object declaration parser, but `object Name =` cannot.
  */
sealed trait Result[+A] {
  def isSuccess: Boolean = isInstanceOf[Success]

  def isRecoverable: Boolean = this == Recoverable

  def getOrElse[B >: A](alternative: => B): B = this match {
    case Success(value) => value
    case _ => alternative
  }
}

case class Success[+A](value: A) extends Result[A]

case object Recoverable extends Result[Nothing]

case object Failure extends Result[Nothing]
