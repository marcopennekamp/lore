package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object FragmentParser {
  import IdentifierParser.identifier
  import TypeParser.typeExpression

  def normalTypeDeclaration[_ : P] = P("type" ~ identifier ~ "=" ~/ typeExpression).map { case (name, expression) =>
    TypeDeclaration(name, expression)
  }

  def labelType[_ : P] = P("type" ~ identifier ~ ("<:" ~/ identifier).?).map { case (name, supertypeName) =>
    LabelTypeDeclaration(name, supertypeName)
  }

  def classType[_ : P] = P("abstract".?.! ~ "class" ~/ identifier ~ ("<:" ~ identifier).?).map { case (abstractKeyword, name, supertypeName) =>
    ClassTypeDeclaration(name, supertypeName, abstractKeyword == "abstract")
  }

  def typeDeclaration[_ : P] = {
    P(normalTypeDeclaration | labelType | classType)
  }

  def parameterDeclaration[_ : P] = {
    P(identifier ~ ":" ~ typeExpression).map { case (name, expr) => ParameterDeclaration(name, expr) }
  }

  def functionDeclaration[_ : P] = {
    P("function" ~/ identifier ~ "(" ~ parameterDeclaration.? ~ ("," ~ parameterDeclaration).rep ~ ")" ~ ("=" ~ "()".!).?).map {
      case (name, firstParameter, restParameters, body) =>
        val parameters = firstParameter.map(_ +: restParameters).getOrElse(restParameters)
        FunctionDeclaration(name, parameters, isAbstract = body.isEmpty)
    }
  }

  def topDeclaration[_ : P] = P(typeDeclaration | functionDeclaration)

  def file[_ : P] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    P(topDeclaration.repX(0, Space.terminators) ~~ Space.WL0 ~~ End)
  }

  def parse(source: String): Seq[TopLevelElement] = {
    fastparse.parse(source, file(_)) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => sys.error("Parsing failure! " + info)
    }
  }
}
