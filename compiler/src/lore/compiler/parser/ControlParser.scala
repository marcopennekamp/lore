package lore.compiler.parser

import lore.compiler.syntax.{TkDedent, TkIndent, TkNewline, Token}
import scalaz.Scalaz.ToOptionIdOps

trait ControlParser { _: Parser =>
  /**
    * Allows the code parsed by `parse` to be contained in an optional indentation section. The boolean passed to
    * `parse` signifies whether an indentation section has been opened. `parse` should usually allow the body to be
    * spread over multiple lines if it's in an indentation section.
    *
    * Returns the result of `parse` if the indentation was valid or not encountered, or `None` if the required dedent
    * wasn't present.
    */
  def withOptionalIndentation[A](parse: Boolean => A): Option[A] =
    peek match {
      case TkIndent =>
        consume()
        val result = parse(true)
        consume() match {
          case TkDedent => result.some
          case _ => None
        }
      case _ => parse(false).some
    }

  /**
    * Parses the given separator token and a following, optional [[TkNewline]] if `allowNewline` is enabled. Returns
    * `true` if the separator was encountered.
    */
  def separatorNl(separatorToken: Token, allowNewline: Boolean = true): Boolean = {
    val found = consumeOnly(separatorToken)
    if (allowNewline) consumeOnly(TkNewline)
    found
  }
}
