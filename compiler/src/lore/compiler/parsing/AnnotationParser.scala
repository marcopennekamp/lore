package lore.compiler.parsing

import fastparse._
import lore.compiler.core.Fragment
import lore.compiler.syntax.DeclNode

/**
  * We will probably formalize annotations down the line, but for now this parser exists exclusively to parse `@where`
  * annotations.
  *
  * Annotations are always terminated by a newline. Hence, annotation whitespace must default to exclude newlines,
  * except when individual sections are separated by commas. This however disallows trailing commas in e.g. `@where`
  * annotations. The alternative would be to make `func`, `act`, etc. into keywords. This would be tenable, but once
  * we add annotations to structs, traits, and types, we'd have to make these into keywords as well. I'd like to avoid
  * that.
  */
class AnnotationParser(nameParser: NameParser)(implicit fragment: Fragment) {

  implicit val whitespace: P[Any] => P[Unit] = Space.WS(_)

  private val typeParameterParser = new TypeParameterParser(nameParser)

  def where[_: P]: P[Vector[DeclNode.TypeVariableNode]] = {
    def parameters = {
      implicit val whitespace: P[Any] => P[Unit] = ScalaWhitespace.whitespace
      typeParameterParser.simpleParameter.rep(1, CharIn(","))
    }
    P("@where" ~~ Space.WS1 ~~ parameters ~~ Space.terminators).map(_.toVector)
  }

}
