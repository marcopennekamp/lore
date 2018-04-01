package lore.parser

import lore.ast._

object FragmentParser extends IgnoreWhitespace {
  import whitespaceApi._
  import fastparse.noApi._
  import IdentifierParser.identifier
  import TypeParser.typeExpression

  val typeDeclaration: P[TopLevelElement] = {
    val normalTypeDeclaration = P("type" ~ identifier ~ "=" ~ typeExpression).map { case (name, expression) =>
      TypeDeclaration(name, expression)
    }
    val labelType = P("type" ~ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) =>
      LabelTypeDeclaration(name, supertypeName)
    }
    P(normalTypeDeclaration | labelType)
  }

  val parameterDeclaration: P[ParameterDeclaration] = {
    P(identifier ~ ":" ~ typeExpression).map { case (name, expr) => ParameterDeclaration(name, expr) }
  }

  val functionDeclaration: P[FunctionDeclaration] = {
    P("function" ~ identifier ~ "(" ~ parameterDeclaration.? ~ ("," ~ parameterDeclaration).rep ~ ")" ~ ("=" ~ "()".!).?).map {
      case (name, firstParameter, restParameters, body) =>
        val parameters = firstParameter.map(_ +: restParameters).getOrElse(restParameters)
        FunctionDeclaration(name, parameters, isAbstract = body.isEmpty)
    }
  }

  val callWith: P[CallWith] = {
    P("call" ~ identifier ~ "with" ~ typeExpression).map { case (functionName, typeExpr) =>
      CallWith(functionName, typeExpr)
    }
  }

  val file: P[Seq[TopLevelElement]] = {
    P(((typeDeclaration | functionDeclaration | callWith) ~ "\n").rep ~ End)
  }

  def parse(source: String): Seq[TopLevelElement] = {
    file.parse(source) match {
      case Parsed.Success(result, _) => result
      case Parsed.Failure(_, _, info) => sys.error("Parsing failure! " + info)
    }
  }
}
