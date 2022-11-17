package lore.compiler

package object parser {
  type BasicParsers = NameParser with IndentationParser with WhitespaceParser
}
