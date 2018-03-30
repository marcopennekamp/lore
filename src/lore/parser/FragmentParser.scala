package lore.parser

import fastparse.WhitespaceApi
import lore.ast._

class FragmentParser() {
  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  object WhitespaceSensitive {
    import fastparse.all._
    val lowercase  = P( CharIn('a' to 'z') )
    val uppercase  = P( CharIn('A' to 'Z') )
    val letter     = P( lowercase | uppercase )
    val digit      = P( CharIn('0' to '9') )

    val identifier: P[String] = P( (letter | "_") ~ (letter | digit | "_").rep ).!
  }

  import fastparse.noApi._
  import White._
  import WhitespaceSensitive._

  val typeDeclaration: P[TypeDeclaration] = {
    P("type" ~ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) => TypeDeclaration(name, supertypeName) }
  }

  val typeExpression: P[TypeExpression] = {
    val intersectionOrVariable = P(identifier ~ ("&" ~ typeExpression).rep).map { case (firstType, intersectionTypes) =>
      if (intersectionTypes.isEmpty) {
        TypeVariable(firstType)
      } else {
        IntersectionTypeExpression((TypeVariable(firstType) +: intersectionTypes).toSet)
      }
    }
    val tuple = P("(" ~ typeExpression ~ ("," ~ typeExpression).rep ~ ")").map { case (firstType, restTypes) =>
      TupleTypeExpression(firstType +: restTypes)
    }
    P(intersectionOrVariable | tuple)
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
