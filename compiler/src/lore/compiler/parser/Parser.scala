package lore.compiler.parser

import lore.compiler.core.{Fragment, Position}
import lore.compiler.feedback.MemoReporter
import lore.compiler.syntax.Node.Index
import lore.compiler.syntax.{TkEnd, Token}
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
  // Input helpers.
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
  def consume(): Token = {
    val token = peek
    token match {
      case _: TkEnd =>
      case _ => offset += 1
    }
    token
  }

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

  def consumeExpect[T <: Token](error: => Feedback)(implicit tag: ClassTag[T]): Result[T] = {
    consume[T] match {
      case success@Success(_) => success
      case Failure =>
        reporter.report(error)
        Failure
    }
  }

  /**
    * Consumes tokens allowed by `isAllowed` as long as the current token and the next token are directly adjacent by
    * index. [[consumeConnectedTokens]] is sensitive to whitespace in the sense that it compares raw indices. Its use
    * should be limited to cases where the lexer should have created a single token, but wasn't able to due to missing
    * context information.
    */
  @StateConservative
  def consumeConnectedTokens(isAllowed: Token => Boolean): Vector[Token] =
    VectorExtension.unfoldOnPreviousElement { previousToken =>
      peek match {
        case candidate: Token if isAllowed(candidate) =>
          previousToken match {
            case Some(previousToken) if candidate.startIndex != previousToken.endIndex + 1 => None
            case _ =>
              consume()
              candidate.some
          }
        case _ => None
      }
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
      if (peek == separator) consume()
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
    * Collects results from `get` and `separator` in an alternating order as long as either is a success.
    * [[collectSepSemantic]] backtracks `get` and `separator` because both are inherently exploratory.
    *
    * TODO (syntax): Share implementation with `collectSep`?
    * TODO (syntax): Provide a variant without `get.backtrack` which will be useful for optimization in cases where
    *                `get` is already state-conservative.
    */
  @StateConservative
  def collectSepSemantic[A, B](
    separator: => Result[B],
    trailingSeparator: => Option[Result[B]] = None,
  )(get: => Result[A]): (Vector[A], Vector[B]) = {
    // TODO (syntax): This needs to be updated similarly to `collectSep`.
    var elements = Vector.empty[A]
    var separators = Vector.empty[B]
    var ended = false

    val nextElement: () => Boolean = () => get.backtrack match {
      case Success(result) =>
        elements :+= result
        true
      case Failure => false
    }

    val nextSeparator: (=> Result[B]) => Boolean = separator => separator.backtrack match {
      case Success(result) =>
        separators :+= result
        true
      case Failure => false
    }

    while (!ended) {
      get.backtrack match {
        case Success(result) =>
          elements :+= result
          separator.backtrack match {
            case Success(result) => separators :+= result
            case Failure => ended = true
          }

        case Failure => ended = true
      }
    }

    trailingSeparator.foreach { trailingSeparator =>
      trailingSeparator.backtrack match {
        case Success(separator) => separators :+= result
        case Failure => ???
      }
    }

    if (allowTrailing) {
      separator.backtrack match {
        case Some(result) => separators :+= result
        case None =>
      }
    }
    (elements, separators)
  }

  /**
    * Surrounds `action` with `left` and `right`, requiring that both `left` and `right` are present. `action` is not
    * backtracked automatically.
    */
  def surround[A](left: => Boolean, right: => Boolean)(action: => Option[A]): Option[A] = {
    if (left) action.filter(_ => right) else None
  }

  // TODO (syntax): Should be inline (Scala 3).
//  def withPosition[T <: PositionedToken](token: T): (T, Position) = {
//    result.map((_, Position(fragment, startOffset, endOffset)))
//  }

  // TODO (syntax): Should be inline (Scala 3).
//  def createPositionFrom(startIndex: Int): Position = Position(fragment, startIndex, endIndex = offset)

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

  implicit class BooleanActionExtension(action: => Boolean) {
    def backtrack: Boolean = backtrackImpl(action, isSuccess => !isSuccess)
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
}
