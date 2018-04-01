package lore.parser

import fastparse.all._

object IdentifierParser {
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val letter     = P( lowercase | uppercase )
  val digit      = P( CharIn('0' to '9') )

  val identifier: P[String] = P( (letter | "_") ~ (letter | digit | "_").rep ).!
}
