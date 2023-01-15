package lore.compiler.parser

import lore.compiler.core.{Fragment, Position}
import lore.compiler.syntax.{PositionedToken, TkEnd, Token}
import scalaz.Scalaz.ToOptionIdOps

import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag

trait Parser {
  implicit def fragment: Fragment

  val tokens: IndexedSeq[Token]

  /**
    * The current offset in [[tokens]].
    */
  protected var offset: Int = 0

  /**
    * An offset-conservative parser does not need backtracking, as the parser will only affect the offset if the run is
    * successful.
    *
    * TODO (syntax): Use this annotation to warn that a `backtrack` call is superfluous. Can we do this in Scala or
    *                IntelliJ natively?
    */
  class OffsetConservative extends StaticAnnotation

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Input helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  @OffsetConservative
  def peek: Token = peek(1)

  @OffsetConservative
  def peek(n: Int): Token = {
    val i = offset + n - 1
    if (i < tokens.length) tokens(i) else TkEnd
  }

  @OffsetConservative
  def consume(): Token = {
    val token = peek
    if (token != TkEnd) offset += 1
    token
  }

  def consumeOnly[T <: Token]()(implicit tag: ClassTag[T]): Option[T] = consume() match {
    case token: T => token.some
    case _ => None
  }

  @OffsetConservative
  def consumeOnly(token: Token): Boolean = {
    if (peek == token) {
      consume()
      true
    } else false
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
    * Collects results from `get` until it returns `None`. `collect` backtracks `get` automatically because it's
    * inherently exploratory.
    *
    * TODO (syntax): Remove (unused).
    */
  @OffsetConservative
  def collect[A](get: => Option[A]): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false
    while (!ended) {
      get.backtrack match {
        case Some(result) => results :+= result
        case None => ended = true
      }
    }
    results
  }

  /**
    * Collects results from `get` until it returns `None`, requiring a `separator` between each production.
    * `collectSep` backtracks `get` and `separator` automatically because both are inherently exploratory.
    *
    * Backtracking `get` is for instance important when module declarations are collected. If `get` doesn't backtrack,
    * an incomplete declaration `func ...` might lead to the `func` token being consumed, `get` returning `None`, and
    * this incomplete declaration just being skipped. An invalid declaration might also be accidentally parsed as
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
    * TODO (syntax): Share implementation with `collect`.
    * TODO (syntax): Provide a variant without `get.backtrack` which will be useful for optimization in cases where
    *                `get` is already offset-conservative.
    */
  @OffsetConservative
  def collectSep[A](separator: => Boolean, allowTrailing: Boolean = false)(get: => Option[A]): Vector[A] = {
    var results = Vector.empty[A]
    var ended = false
    while (!ended) {
      get.backtrack match {
        case Some(result) =>
          results :+= result
          if (!separator.backtrack) ended = true
        case None => ended = true
      }
    }
    if (allowTrailing) separator.backtrack
    results
  }

  /**
    * Collects results from `get` and `separator` until `get` returns `None`, requiring a `separator` between each
    * production. `collectSep` backtracks `get` and `separator` automatically because both are inherently exploratory.
    *
    * TODO (syntax): Share implementation with `collectSep`?
    * TODO (syntax): Provide a variant without `get.backtrack` which will be useful for optimization in cases where
    *                `get` is already offset-conservative.
    */
  @OffsetConservative
  def collectSepSemantic[A, B](
    separator: => Option[B],
    allowTrailing: Boolean = false,
  )(get: => Option[A]): (Vector[A], Vector[B]) = {
    var elements = Vector.empty[A]
    var separators = Vector.empty[B]
    var ended = false
    while (!ended) {
      get.backtrack match {
        case Some(result) =>
          elements :+= result
          separator.backtrack match {
            case Some(result) => separators :+= result
            case None => ended = true
          }
        case None => ended = true
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

  implicit class PositionedTokenExtension[T <: PositionedToken](token: T) {
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
    def backtrack: Boolean = {
      val savedOffset = offset
      val result = action
      if (!result) offset = savedOffset
      result
    }
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
    def backtrack: Option[A] = {
      val savedOffset = offset
      val result = action
      if (result.isEmpty) offset = savedOffset
      result
    }
  }
}
