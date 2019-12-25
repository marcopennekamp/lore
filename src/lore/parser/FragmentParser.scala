package lore.parser

import lore.ast._
import fastparse._

object FragmentParser extends IgnoreWhitespace {
  import IdentifierParser.identifier
  import TypeParser.typeExpression

  def normalTypeDeclaration[_ : P] = P("type" ~ identifier ~ "=" ~ typeExpression).map { case (name, expression) =>
    TypeDeclaration(name, expression)
  }

  def labelType[_ : P] = P("type" ~ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) =>
    LabelTypeDeclaration(name, supertypeName)
  }

  def typeDeclaration[_ : P]: P[TopLevelElement] = {
    P(normalTypeDeclaration | labelType)
  }

  def parameterDeclaration[_ : P]: P[ParameterDeclaration] = {
    P(identifier ~ ":" ~ typeExpression).map { case (name, expr) => ParameterDeclaration(name, expr) }
  }

  def functionDeclaration[_ : P]: P[FunctionDeclaration] = {
    P("function" ~ identifier ~ "(" ~ parameterDeclaration.? ~ ("," ~ parameterDeclaration).rep ~ ")" ~ ("=" ~ "()".!).?).map {
      case (name, firstParameter, restParameters, body) =>
        val parameters = firstParameter.map(_ +: restParameters).getOrElse(restParameters)
        FunctionDeclaration(name, parameters, isAbstract = body.isEmpty)
    }
  }

  def file[_ : P]: P[Seq[TopLevelElement]] = {
    P(((typeDeclaration | functionDeclaration) ~/ "\n").rep ~ End)
  }

  def parse(source: String): Seq[TopLevelElement] = {
    fastparse.parse(source, file(_)) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => sys.error("Parsing failure! " + info)
    }
  }
}
