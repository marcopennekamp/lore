package lore.compiler.parser

import lore.compiler.syntax.{TkDedent, TkIndent, TkNewline, Token}

trait ControlParser { _: Parser =>
  /**
    * Allows the code parsed by `parse` to be contained in an optional indentation section. The boolean passed to
    * `parse` signifies whether an indentation section has been opened. `parse` should usually allow the body to be
    * spread over multiple lines if it's in an indentation section.
    *
    * Returns the result of `parse` if the indentation was valid or not encountered, or reports an error and returns
    * [[Failure]] if the required dedent wasn't present. (We return [[Failure]] instead of [[Recoverable]] because
    * [[withOptionalIndentation]] already has a built-in option from which no backtracking should be needed. If `parse`
    * itself returns a [[Result]], that can be backtracked separately.)
    *
    * TODO (syntax): Remove?
    */
  def withOptionalIndentation[A](parse: Boolean => Result[A]): Result[A] =
    peek match {
      case TkIndent =>
        consume()
        val result = parse(true)
        consume() match {
          case TkDedent => result
          case _ =>
            // TODO (syntax): Report error.
            Failure
        }
      case _ => parse(false)
    }

  /**
    * Opens an optional indentation section and returns whether an indentation was opened.
    */
  @StateConservative
  def openOptionalIndentation(): Boolean = consumeIf[TkIndent]

  /**
    * Closes an indentation section and returns whether it was successful. Reports an error if it wasn't.
    */
  def closeIndentation(): Boolean = {
    val isClosed = consumeIf[TkDedent]
    if (!isClosed) {
      // TODO (syntax): Report error.
    }
    isClosed
  }

  /**
    * Parses the given separator and a following, optional [[TkNewline]] if `allowNewline` is enabled. Returns `true`
    * if the separator was encountered.
    */
  def separatorNl(separator: => Boolean, allowNewline: Boolean = true): Boolean = {
    val found = separator
    if (allowNewline) consumeIf[TkNewline]
    found
  }
}
