package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object FragmentParser {
  import LexicalParser.identifier
  import TypeParser.typeExpression

  def normalTypeDeclaration[_: P]: P[TypeDeclNode.AliasNode] = P("type" ~/ identifier ~ "=" ~ typeExpression).map { case (name, expression) =>
    TypeDeclNode.AliasNode(name, expression)
  }

  def labelType[_: P]: P[TypeDeclNode.LabelNode] = P("label" ~/ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) =>
    TypeDeclNode.LabelNode(name, supertypeName)
  }

  def classType[_: P]: P[TypeDeclNode.ClassNode] = P("abstract".?.! ~ "class" ~/ identifier ~ ("<:" ~ identifier).?).map { case (abstractKeyword, name, supertypeName) =>
    TypeDeclNode.ClassNode(name, supertypeName, abstractKeyword == "abstract", List.empty, List.empty) // TODO: Pass properties and constructors.
  }

  def typeDeclaration[_: P]: P[TypeDeclNode] = {
    P(normalTypeDeclaration | labelType | classType)
  }

  def parameterDeclaration[_: P]: P[DeclNode.ParameterNode] = {
    P(identifier ~ ":" ~ typeExpression).map { case (name, expr) => DeclNode.ParameterNode(name, expr) }
  }

  def functionDeclaration[_: P]: P[DeclNode.FunctionNode] = {
    P("function" ~/ identifier ~ "(" ~ parameterDeclaration.? ~ ("," ~ parameterDeclaration).rep ~ ")" ~ ("=" ~ "()".!).?).map {
      case (name, firstParameter, restParameters, body) =>
        val parameters = firstParameter.map(_ +: restParameters).getOrElse(restParameters).toList
        DeclNode.FunctionNode(name, parameters, isAbstract = body.isEmpty, TypeExprNode.UnitNode, ExprNode.UnitNode) // TODO: Pass output type and body.
    }
  }

  def topDeclaration[_: P]: P[DeclNode] = P(typeDeclaration | functionDeclaration)

  def file[_: P]: P[Seq[DeclNode]] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    Space.WL0 ~~ P(topDeclaration.repX(0, Space.terminators) ~~ Space.WL0 ~~ End)
  }

  def parse(source: String): Option[Seq[DeclNode]] = {
    fastparse.parse(source, file(_)) match {
      case Parsed.Success(result, _) => Some(result)
      case Parsed.Failure(label, index, extra) =>
        println(s"Parsing failure: ${extra.trace().aggregateMsg}")
        None
    }
  }
}
