package lore.compiler.parser

import lore.compiler.core.{Fragment, Position}
import lore.compiler.feedback.{Feedback, MemoReporter, ParserFeedback}
import lore.compiler.syntax.Node.Index
import lore.compiler.syntax._
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Scalaz.ToOptionIdOps

import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag

trait Parser {
  implicit def fragment: Fragment

  val tokens: IndexedSeq[Token]

  /**
    * The current offset in [[tokens]].
    *
    * [[offset]] needs to be backtracked as it's part of the parser state.
    */
  protected var offset: Index = 0

  /**
    * The feedback collected by the parser so far.
    *
    * [[reporter]]'s state needs to be backtracked as it's part of the parser state.
    */
  protected var reporter: MemoReporter = MemoReporter()

  /**
    * A state-conservative parser does not need backtracking, as the parser will only affect the run's state if the run
    * is successful.
    *
    * TODO (syntax): Use this annotation to warn that a `backtrack` call is superfluous. Can we do this in Scala or
    *                IntelliJ natively?
    */
  class StateConservative extends StaticAnnotation

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Single-token input helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  @StateConservative
  def peek: Token = peek(1)

  @StateConservative
  def peek(n: Int): Token = {
    val i = offset + n - 1
    if (i < tokens.length) tokens(i) else TkEnd(fragment.input.length)
  }

  @StateConservative
  def peekIs[T <: Token](implicit tag: ClassTag[T]): Boolean = peekIs[T](1)

  @StateConservative
  def peekIs[T <: Token](n: Int)(implicit tag: ClassTag[T]): Boolean = peek.isInstanceOf[T]

  @StateConservative
  def skip(): Unit = skip(1)

  @StateConservative
  def skip(n: Int): Unit = {
    offset += n
  }

  @StateConservative
  def consume(): Token = {
    val token = peek
    skip()
    token
  }

  // TODO (syntax): Check if `ClassTag` has an impact on parser performance. An alternative could be introducing a
  //                "token type" enum. This would make writing certain utility functions easier. The token type could
  //                have its token as a type argument, so that functions like `consume` still return the narrowest
  //                possible type.
  @StateConservative
  def consume[T <: Token](implicit tag: ClassTag[T]): Result[T] = consume() match {
    case token: T => token.success
    case _ => Failure
  }

  @StateConservative
  def consumeIf[T <: Token](implicit tag: ClassTag[T]): Boolean = consume() match {
    case _: T => true
    case _ => false
  }

  def consumeExpect[T <: Token](error: => Feedback)(implicit tag: ClassTag[T]): Result[T] =
    consume[T] match {
      case success@Success(_) => success
      case Failure =>
        reporter.report(error)
        Failure
    }

  def consumeExpect[T <: Token](implicit tag: ClassTag[T]): Result[T] =
    consumeExpect[T](ParserFeedback.TokenExpected[T](peek.position))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Special token handling.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Opens an optional indentation section and returns whether an indentation was opened. [[openOptionalIndentation]]
    * skips a newline if present with indentation, but does not require it.
    */
  @StateConservative
  def openOptionalIndentation(): Boolean = {
    if (peekIs[TkNewline] && peekIs[TkIndent]) {
      skip(2)
      true
    } else if (peekIs[TkIndent]) {
      skip()
      true
    } else false
  }

  /**
    * Attempts to close an indentation section. Reports an error and returns [[Failure]] if it wasn't closed.
    *
    * TODO (syntax): Should this consume a newline before the dedent if present like `openOptionalIndentation`?
    */
  def closeIndentation(): Result[TkDedent] = consumeExpect[TkDedent]

  def peekIsWithPossibleIndent(isToken: Token => Boolean): Boolean =
    isToken(peek) || peekIs[TkIndent] && isToken(peek(2)) || peekIs[TkNewline] && peekIs[TkIndent](2) && isToken(peek(3))

  def peekIsWithPossibleDedent(isToken: Token => Boolean): Boolean =
    isToken(peek) || peekIs[TkDedent] && isToken(peek(2)) || peekIs[TkNewline] && peekIs[TkDedent](2) && isToken(peek(3))

  def bracketList[A](get: => Result[A]): Result[(Vector[A], Position)] =
    collectSepEnclosedWithOptionalIndentation[A, TkBracketLeft, TkBracketRight](consumeIf[TkComma]) { get }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Separators and collectors.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Parses the given separator and a following, optional [[TkNewline]] if `allowNewline` is enabled. Returns `true`
    * if the separator was encountered.
    */
  def separatorNl(separator: => Boolean, allowNewline: Boolean = true): Boolean = {
    val found = separator
    if (allowNewline) consumeIf[TkNewline]
    found
  }

  /**
    * Repeats `action` until it returns `false`. Returns `true` if at least one action was successfully executed.
    * `repeat` does NOT backtrack `action` by default.
    */
  def repeat(action: => Boolean): Boolean = {
    var consumed = false
    while (action) {
      consumed = true
    }
    consumed
  }

  // TODO (syntax): Remove (unused).
  // TODO (syntax): Share implementation with `repeat`.
  def repeatSep(separator: Token)(action: => Boolean): Boolean = {
    var consumed = false
    while (action) {
      consumed = true
      if (peek == separator) skip()
      else return true
    }
    consumed
  }

  /**
    * Collects results from `get` as long as `hasNext` is `true`. `get` may lead to a [[Failure]], which is not
    * backtracked by this function.
    */
  def collectLookahead[A](hasNext: => Boolean)(get: => Result[A]): Result[Vector[A]] = {
    var results = Vector.empty[A]
    while (hasNext) {
      get match {
        case Success(result) => results :+= result
        case Failure => return Failure
      }
    }
    results.success
  }

  /**
    * Collects results from `get` as long as [[peek]] is a token of type `T`. `get` may lead to a [[Failure]], which is
    * not backtracked by this function.
    */
  def collectLookaheadIs[A, T <: Token](get: T => Result[A])(implicit tag: ClassTag[T]): Result[Vector[A]] =
    collectLookahead(peekIs[T]) { get(consume().asInstanceOf[T]) }

  /**
    * Collects results from `get` as long as `get` is a success, backtracking `get` in the process.
    */
  @StateConservative
  def collectBacktrack[A](get: => Result[A]): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false
    while (!ended) {
      get.backtrack match {
        case Success(result) => results :+= result
        case Failure => ended = true
      }
    }
    results
  }

  /**
    * Collects results from `get` as long as `hasNext` is `true`. `get` may lead to a [[Failure]], which is not
    * backtracked by this function. After each `get`, a `separator` is parsed with backtracking. If `allowTrailing` is
    * `false`, [[collectSepLookahead]] will report an error and return a failure if `hasNext` after any `separator` is
    * `false`.
    */
  def collectSepLookahead[A](
    hasNext: => Boolean,
    separator: => Boolean,
    allowTrailing: Boolean = true,
  )(get: => Result[A]): Result[Vector[A]] = {
    var results = Vector.empty[A]

    var ended = false
    while (!ended && hasNext) {
      get match {
        case Success(result) => results :+= result
        case Failure => return Failure
      }

      if (separator.backtrack) {
        if (!allowTrailing && !hasNext) {
          // TODO (syntax): Report error (trailing separator not allowed).
          return Failure
        }
      } else {
        ended = true
      }
    }

    results.success
  }

  /**
    * Collects results from `get` as long as [[peek]] is a token of type `T`. `get` may lead to a [[Failure]], which is
    * not backtracked by this function. After each `get`, a `separator` is parsed with backtracking. If `allowTrailing`
    * is `false`, [[collectSepLookahead]] will report an error and return a failure if the token after `separator` is
    * not of type `T`.
    */
  def collectSepLookaheadIs[A, T <: Token](
    separator: => Boolean,
    allowTrailing: Boolean = true,
  )(get: T => Result[A])(implicit tag: ClassTag[T]): Result[Vector[A]] =
    collectSepLookahead(peekIs[T], separator, allowTrailing) { get(consume().asInstanceOf[T]) }

  /**
    * Collects results from `get` as long as it is a success, requiring a `separator` between each production. If a
    * `trailingSeparator` is specified, [[collectSepBacktrack]] attempts to consume `trailingSeparator` after the last
    * element.
    *
    * [[collectSepBacktrack]] backtracks `get`, `separator`, and `trailingSeparator`.
    *
    * Backtracking `get` is for instance important when module declarations are collected. If `get` doesn't backtrack,
    * an incomplete declaration `func ...` might lead to the `func` token being consumed, `get` returning `Recoverable`,
    * and this incomplete declaration just being skipped. An invalid declaration might also be accidentally parsed as
    * something else, for example:
    *
    * {{{
    * module Functions
    *   use module Test
    *     func hello(): String = 'hello'
    * }}}
    *
    * If the parser doesn't backtrack, `use` might be skipped and `module Test` might still be parsed as a member of
    * module `Functions`. This is because module members are parsed sequentially after imports.
    *
    * TODO (syntax): Provide a variant without `get.backtrack` for better error support. (Only the separator will be
    *                "exploratory", with errors reported in `get` going through to the parent parser.)
    */
  @StateConservative
  def collectSepBacktrack[A](
    separator: => Boolean,
    trailingSeparator: => Boolean = false,
  )(get: => Result[A]): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false

    def next(): Boolean = get.backtrack match {
      case Success(result) =>
        results :+= result
        true
      case Failure => false
    }

    // Consume the separator and then the element in one backtrack grouping. Otherwise, a trailing separator may be
    // consumed by the wrong separator rule. This is also the reason why the first element is consumed outside the
    // `while` loop.
    ended = !next()
    while (!ended) {
      ended = !(separator && next()).backtrack
    }
    trailingSeparator.backtrack
    results
  }

  /**
    * Collects tokens allowed by `isAllowed` as long as the current token and the next token are directly adjacent by
    * index. [[collectConnectedTokens]] is sensitive to whitespace in the sense that it compares raw indices. Its use
    * should be limited to cases where the lexer should have created a single token, but wasn't able to due to missing
    * context information.
    */
  @StateConservative
  def collectConnectedTokens(isAllowed: Token => Boolean): Vector[Token] =
    VectorExtension.unfoldOnPreviousElement { previousToken =>
      peek match {
        case candidate: Token if isAllowed(candidate) =>
          previousToken match {
            case Some(previousToken) if candidate.startIndex != previousToken.endIndex + 1 => None
            case _ =>
              skip()
              candidate.some
          }
        case _ => None
      }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Enclosures.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Encloses `get` in the given opening and closing tags. An optional indentation is allowed immediately after the
    * opening token. On success, returns the result and the position from the opening to the closing token.
    *
    * @param get `Boolean` informs whether an indentation section has been opened for the enclosure.
    */
  def encloseWithOptionalIndentation[
    A,
    OpeningToken <: Token,
    ClosingToken <: Token,
  ](
    get: Boolean => Result[A],
  )(
    implicit openingTag: ClassTag[OpeningToken],
    closingTag: ClassTag[ClosingToken],
  ): Result[(A, Position)] = {
    val openingToken = consumeExpect[OpeningToken].getOrElse(return Failure)
    val isIndented = openOptionalIndentation()

    val result = get(isIndented).getOrElse(return Failure)

    if (isIndented) closeIndentation().getOrElse(return Failure)
    val closingToken = consumeExpect[ClosingToken].getOrElse(return Failure)

    (result, openingToken.position.to(closingToken.position)).success
  }

  /**
    * Collects `get` enclosed in the given opening and closing tokens with lookahead, separated by `separator`. An
    * optional indentation is allowed immediately after the opening token. On success, returns the collected elements
    * and the position from the opening to the closing token on success.
    */
  def collectSepEnclosedWithOptionalIndentation[
    A,
    OpeningToken <: Token,
    ClosingToken <: Token,
  ](
    separator: => Boolean,
  )(
    get: => Result[A],
  )(
    implicit openingTag: ClassTag[OpeningToken],
    closingTag: ClassTag[ClosingToken],
  ): Result[(Vector[A], Position)] =
    encloseWithOptionalIndentation[Vector[A], OpeningToken, ClosingToken] { isIndented =>
      def hasNext =
        if (isIndented) peekIsWithPossibleDedent(peekIs[ClosingToken])
        else peekIs[ClosingToken]

      collectSepLookahead(
        !hasNext,
        separatorNl(separator, allowNewline = isIndented),
      )(get)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Backtracking.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit class BooleanActionExtension(action: => Boolean) {
    def backtrack: Boolean = backtrackImpl(action, isSuccess => !isSuccess)
  }

  implicit class OptionActionExtension[A](action: => Option[A]) {
    def backtrack: Option[A] = backtrackImpl(action, _.isEmpty)
  }

  implicit class ResultActionExtension[A](action: => Result[A]) {
    def backtrack: Result[A] = backtrackImpl(action, _.isRecoverable)

    /**
      * Takes the result of `action` if it's a success or failure, or otherwise backtracks `action` and defers to
      * `other`.
      *
      * TODO (syntax): Should be inline (Scala 3).
      */
    def |[B >: A](other: => Result[B]): Result[B] = action.backtrack match {
      case result: Success[A] => result
      case Failure => other
    }
  }

  // TODO (syntax): Should be inline (Scala 3).
  private def backtrackImpl[A](action: => A, shouldBacktrack: A => Boolean): A = {
    val savedOffset = offset
    val savedReporterState = reporter.currentState()
    val result = action
    if (shouldBacktrack(result)) {
      offset = savedOffset
      reporter.restoreState(savedReporterState)
    }
    result
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Extensions.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit class TokenExtension[T <: Token](token: T) {
    def position: Position = Position(fragment, token.startIndex, token.endIndex)

    def withPosition: (T, Position) = (token, position)
  }

  implicit class AnyExtension(any: Any) {
    /**
      * Ignores the result on the left.
      *
      * TODO (syntax): Should be inline (Scala 3).
      */
    def *>[A](other: A): A = other
  }

  implicit class AExtension[A](value: A) {
    /**
      * Ignores the result on the right.
      *
      * TODO (syntax): Should be inline (Scala 3).
      */
    def <*(other: Any): A = value

    /**
      * Wraps [[value]] as a successful result.
      */
    def success: Success[A] = Success(value)
  }

  implicit class BooleanExtension(value: Boolean) {
    /**
      * Maps to `option` if `value` is true and `None` otherwise.
      *
      * TODO (syntax): Should be inline (Scala 3).
      */
    def &>[A](option: => Option[A]): Option[A] = if (value) option else None
  }

  implicit class OptionExtension[A](option: Option[A]) {
    /**
      * Maps to `option` if `condition` is true and `None` otherwise.
      *
      * TODO (syntax): Should be inline (Scala 3).
      */
    def <&(condition: => Boolean): Option[A] = if (condition) option else None

    /**
      * Takes `option` if it is `Some` or `other` otherwise.
      *
      * TODO (syntax): Should be inline (Scala 3).
      * TODO (syntax): Can't this backtrack the left-hand side by default?
      */
    def |[B >: A](other: => Option[B]): Option[B] = option orElse other
  }
}
