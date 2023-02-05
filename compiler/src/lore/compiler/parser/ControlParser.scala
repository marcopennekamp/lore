package lore.compiler.parser

import lore.compiler.syntax.{TkDedent, TkIndent, TkNewline, Token}

trait ControlParser { _: Parser =>
  /**
    * TODO (syntax): Update documentation.
    *
    * Allows the code parsed by `parse` to be contained in an optional indentation section. The boolean passed to
    * `parse` signifies whether an indentation section has been opened. `parse` should usually allow the body to be
    * spread over multiple lines if it's in an indentation section.
    *
    * Returns the result of `parse` if the indentation was valid or not encountered, or reports an error and returns
    * [[Failure]] if the required dedent wasn't present.
    *
    * TODO (syntax): Remove?
    */
  def withOptionalIndentation[A](parse: Boolean => Result[A]): Result[A] =
    peek match {
      case TkIndent(_) =>
        consume()
        val result = parse(true)
        consume() match {
          case TkDedent(_) => result
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
    * Attempts to close an indentation section and reports an error and returns [[Failure]] if it wasn't closed.
    */
  def closeIndentation(): Result[Unit] = {
    val isClosed = consumeIf[TkDedent]
    if (!isClosed) {
      // TODO (syntax): Report error.
      Failure
    } else Success.empty
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
