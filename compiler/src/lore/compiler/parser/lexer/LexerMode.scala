package lore.compiler.parser.lexer

/**
  * [[LexerMode]] allows the lexer to switch between lexing code and strings with interpolation. The lexer keeps a
  * stack of modes to keep track of brace depths. Every `{` encountered by [[CodeLexerMode]] (e.g. opening a shape) and
  * every `${` encountered by [[StringLexerMode]] will push a [[CodeLexerMode]], while a `}` pops the [[CodeLexerMode]]
  * mode. The lexer recognizes Lore code as a simplified context-free language, but it should still have linear
  * performance given that it doesn't backtrack.
  */
sealed trait LexerMode

case object CodeLexerMode extends LexerMode
case object StringLexerMode extends LexerMode
