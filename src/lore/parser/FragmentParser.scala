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

  val typeDeclaration: P[TopLevelElement] = {
    val normalTypeDeclaration = P("type" ~ identifier ~ "=" ~ typeExpression).map { case (name, expression) =>
      TypeDeclaration(name, expression)
    }
    val labelType = P("type" ~ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) =>
      LabelTypeDeclaration(name, supertypeName)
    }
    P(normalTypeDeclaration | labelType)
  }

  val sumType: P[SumTypeExpression] = {
    val innerTypeExpression = P(intersectionType | tupleType | typeVariable | enclosedType)
    P(identifier ~ ("|" ~/ innerTypeExpression).rep(min = 1)).map { case (firstType, types) =>
      SumTypeExpression((TypeVariable(firstType) +: types).toSet)
    }
  }

  val intersectionType: P[IntersectionTypeExpression] = {
    val innerTypeExpression = P(tupleType | typeVariable | enclosedType)
    P(identifier ~ ("&" ~ innerTypeExpression).rep(min = 1)).map { case (firstType, types) =>
      IntersectionTypeExpression((TypeVariable(firstType) +: types).toSet)
    }
  }

  val tupleType: P[TupleTypeExpression] = {
    P("(" ~ typeExpression ~ ("," ~ typeExpression).rep ~ ")").map { case (firstType, restTypes) =>
      TupleTypeExpression(firstType +: restTypes)
    }
  }

  val typeVariable: P[TypeVariable] = P(identifier).map(TypeVariable)

  val enclosedType: P[TypeExpression] = P("(" ~ typeExpression ~ ")")

  val typeExpression: P[TypeExpression] = P(sumType | intersectionType | tupleType | typeVariable | enclosedType)

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
