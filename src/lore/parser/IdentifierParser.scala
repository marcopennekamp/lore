package lore.parser

import fastparse._

object IdentifierParser extends IgnoreWhitespace {
  def lowercase[_ : P]  = P( CharIn("a-z") )
  def uppercase[_ : P]  = P( CharIn("A-Z") )
  def letter[_ : P]     = P( lowercase | uppercase )
  def digit[_ : P]      = P( CharIn("0-9") )

  def identifier[_ : P]: P[String] = P( (letter | "_") ~ (letter | digit | "_").rep ).!
}
