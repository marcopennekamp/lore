package lore.compiler.parser

trait WhitespaceParser { _: Parser =>
  /**
    * Consumes whitespace except for newlines, returning `true` if at least one character was consumed.
    */
  @OffsetConservative
  def ws(): Boolean = repeat(spacesOrTabs() || comment())

  /**
    * Consumes whitespace including newlines, returning `true` if at least one character was consumed.
    */
  @OffsetConservative
  def wl(): Boolean = repeat(ws() || newline())

  @OffsetConservative
  def spacesOrTabs(): Boolean = charsWhile(c => c == ' ' || c == '\t')

  @OffsetConservative
  def newline(): Boolean = character('\n') || word("\r\n")

  @OffsetConservative
  def blankLines(): Boolean = repeat((ws() *> newline()).backtrack)

  /**
    * Moves the cursor to the next non-blank line.
    */
  def nextNonBlankLine(): Boolean = {
    if (ws() *> newline()) {
      // Ignore subsequent blank lines.
      blankLines()
      true
    } else false
  }

  // TODO (syntax): Support block comments.
  @OffsetConservative
  def comment(): Boolean = lineComment()

  @OffsetConservative
  def lineComment(): Boolean = {
    if (word("--")) {
      while (peek != '\n' && (peek != '\r' || peek(2) != '\n')) {
        consume()
      }
      true
    } else false
  }
}
