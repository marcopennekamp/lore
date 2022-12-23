package lore.compiler.parser

import lore.compiler.utils.CollectionExtensions.VectorExtension

trait IndentationParser { _: Parser with WhitespaceParser =>
  /**
    * Attempts to open a new indentation block on the next non-blank line. Returns the next line's indentation if it's
    * higher than `parentIndentation`. `indent` expects to be at the end of the line; if instead another token is
    * encountered, `indent` will return `None`.
    */
  def indent(parentIndentation: Int): Option[Int] = {
    if (!nextNonBlankLine()) return None

    val startOffset = offset
    chars(' ')
    val indentation = offset - startOffset
    println(s"Branch indentation ($parentIndentation) at ${fragment.input.prettyIndex(startOffset)}: $indentation indented")
    if (parentIndentation < indentation) Some(indentation) else None
  }

  /**
    * Consumes whitespace until the first non-blank line and checks that the current indentation is equal to the given
    * indentation. The parser succeeds only if a newline is encountered.
    */
  def nli(indentation: Int): Boolean = {
    if (!nextNonBlankLine()) {
      println(s"Invalid exact indentation ($indentation) at ${fragment.input.prettyIndex(offset)}: no next non-blank line found")
      return false
    }

    val count = countIndentation()
    if (indentation == count) true
    else {
      println(s"Invalid exact indentation ($indentation) at ${fragment.input.prettyIndex(offset)}: $indentation != $count")
      false
    }
  }

  /**
    * Like `wl` with the following difference: if a new line should be consumed, the indentation of the first non-blank
    * line must be greater than or equal to the given indentation. `wlmi` may just consume whitespace on the same line
    * until it encounters the next token; the indentation is only checked if a newline is encountered and `wlmi` only
    * consumes the next line if the minimum indentation is ensured. Returns `true` if at least one character was
    * consumed and the indentation is valid.
    *
    * `wlmi` should be used with statements that need to have some minimum indentation, for example with types such as:
    *
    * <pre>
    * val abc: Either[
    * String,
    * Int,
    * ] =
    * Left("String")
    * </pre>
    *
    * This is a matter of enforcing consistency for indentation-sensitive expressions that might follow the type, even
    * if the type expression itself could be parsed without paying attention to indentation. In the example above, if
    * `]` wasn't forced to at least be aligned with `val abc`, the block starter `=` would be misaligned.
    */
  def wlmi(indentation: Int): Boolean = (nextNonBlankLine() && indentation <= countIndentation()).backtrack || ws()

  /**
    * Like `wl` with the following difference: if a new line should be consumed, the indentation of the first non-blank
    * line must be greater than the given indentation. `wlgi` may just consume whitespace on the same line until it
    * encounters the next token; the indentation is only checked if a newline is encountered and `wlgi` onlz consumes
    * the next line if the minimum indentation is ensured. Returns `true` if at least one character was consumed and
    * the indentation is valid.
    *
    * `wlgi` should be used with multi-line expressions that don't open a new indentation block, such as:
    *
    * <pre>
    * val a = 1 + 2 +
    *   3 + 4
    * </pre>
    *
    * The `3 + 4` belongs to the larger expression `1 + 2 + 3 + 4`. `3 + 4` is not contained in a block, but its
    * indentation must still be checked so that it isn't part of the previous block.
    */
  def wlgi(indentation: Int): Boolean = (nextNonBlankLine() && indentation < countIndentation()).backtrack || ws()

  private def countIndentation(): Int = {
    val startOffset = offset
    chars(' ')
    offset - startOffset
  }

  def collectSepWlmi[A](separator: => Boolean, indentation: Int, allowTrailing: Boolean = false)(get: => Option[A]): Vector[A] =
    collectSep(wlmi(indentation) *> separator <* wlmi(indentation), allowTrailing)(get)

  def collectSepWlgi[A](separator: => Boolean, indentation: Int, allowTrailing: Boolean = false)(get: => Option[A]): Vector[A] =
    collectSep(wlgi(indentation) *> separator <* wlgi(indentation), allowTrailing)(get)

  def surroundWlmi[A](left: => Boolean, right: => Boolean, indentation: Int)(get: => Option[A]): Option[A] =
    // No backtracking needed because `left` and `right` neatly close off the whitespaces on either side.
    surround(left <* wlmi(indentation), wlmi(indentation) *> right)(get)

  def enclosedWlmi[A](
    left: => Boolean,
    right: => Boolean,
    separator: => Boolean,
    indentation: Int,
    minSize: Int = 0,
  )(get: => Option[A]): Option[Vector[A]] = surroundWlmi(left, right, indentation) {
    collectSepWlmi(separator, indentation, allowTrailing = true)(get).takeMinSize(minSize)
  }

  def enclosedInParenthesesWlmi[A](indentation: Int, minSize: Int = 0)(get: => Option[A]): Option[Vector[A]] =
    enclosedWlmi(character('('), character(')'), character(','), indentation, minSize)(get)

  def enclosedInBracketsWlmi[A](indentation: Int, minSize: Int = 0)(get: => Option[A]): Option[Vector[A]] =
    enclosedWlmi(character('['), character(']'), character(','), indentation, minSize)(get)
}
