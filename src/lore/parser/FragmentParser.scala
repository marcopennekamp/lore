package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

/**
  * The parsers contained in this parser collection all parse top-level declarations that can occur in a
  * Lore file (fragment). These are those nodes declared in [[DeclNode]].
  */
object FragmentParser {
  import LexicalParser.identifier
  import StatementParser.{expression, block}
  import TypeParser.typeExpression

  def parse(source: String): Option[Seq[DeclNode]] = {
    fastparse.parse(source, fragment(_)) match {
      case Parsed.Success(result, _) => Some(result)
      case Parsed.Failure(label, index, extra) =>
        println(s"Parsing failure: ${extra.trace().aggregateMsg}")
        None
    }
  }

  def fragment[_: P]: P[Seq[DeclNode]] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    P(Space.WL ~~ topDeclaration.repX(0, Space.terminators) ~~ Space.WL ~~ End)
  }

  def topDeclaration[_: P]: P[DeclNode] = P(functionDeclaration | actionDeclaration | typeDeclaration)

  def functionDeclaration[_: P]: P[DeclNode.FunctionNode] = {
    P("function" ~/ identifier ~ parameters ~ ":" ~ typeExpression ~ ("=" ~ expression).?).map(DeclNode.FunctionNode.tupled)
  }

  def actionDeclaration[_: P]: P[DeclNode.FunctionNode] = {
    P("action" ~/ identifier ~ parameters ~ block.?).map {
      case (name, parameters, body) => DeclNode.FunctionNode(name, parameters, TypeExprNode.UnitNode, body)
    }
  }

  def parameters[_: P]: P[List[DeclNode.ParameterNode]] = {
    def parameter = P(identifier ~ ":" ~ typeExpression).map(DeclNode.ParameterNode.tupled)
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toList)
  }

  def typeDeclaration[_: P]: P[TypeDeclNode] = P(normalTypeDeclaration | labelType | classType)

  def normalTypeDeclaration[_: P]: P[TypeDeclNode.AliasNode] = P("type" ~/ identifier ~ "=" ~ typeExpression).map { case (name, expression) =>
    TypeDeclNode.AliasNode(name, expression)
  }

  def labelType[_: P]: P[TypeDeclNode.LabelNode] = P("label" ~/ identifier ~ ("<:" ~ identifier).?).map { case (name, supertypeName) =>
    TypeDeclNode.LabelNode(name, supertypeName)
  }

  def classType[_: P]: P[TypeDeclNode.ClassNode] = P("abstract".?.! ~ "class" ~/ identifier ~ ("<:" ~ identifier).?).map { case (abstractKeyword, name, supertypeName) =>
    TypeDeclNode.ClassNode(name, supertypeName, abstractKeyword == "abstract", List.empty, List.empty) // TODO: Pass properties and constructors.
  }
}
