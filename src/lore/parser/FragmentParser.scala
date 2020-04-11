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
  import StatementParser.{statement, expression, block, arguments}
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

  private def topDeclaration[_: P]: P[DeclNode] = P(function | action | typeDeclaration)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def function[_: P]: P[DeclNode.FunctionNode] = {
    P("function" ~/ identifier ~ parameters ~ ":" ~ typeExpression ~ ("=" ~ expression).?).map(DeclNode.FunctionNode.tupled)
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    P("action" ~/ identifier ~ parameters ~ block.?).map {
      case (name, parameters, body) => DeclNode.FunctionNode(name, parameters, TypeExprNode.UnitNode, body)
    }
  }

  private def parameters[_: P]: P[List[DeclNode.ParameterNode]] = {
    def parameter = P(identifier ~ ":" ~ typeExpression).map(DeclNode.ParameterNode.tupled)
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toList)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(alias | label | `class` | entity)

  private def alias[_: P]: P[TypeDeclNode.AliasNode] = P("type" ~/ identifier ~ "=" ~ typeExpression).map(TypeDeclNode.AliasNode.tupled)
  private def label[_: P]: P[TypeDeclNode.LabelNode] = P("label" ~/ identifier ~ `extends`).map(TypeDeclNode.LabelNode.tupled)

  private def `class`[_: P]: P[TypeDeclNode.ClassNode] = {
    P("abstract".?.! ~ "class" ~/ identifier ~ `extends` ~ classBody(property)).map {
      case (abstractKeyword, name, supertypeName, (properties, constructors)) =>
        TypeDeclNode.ClassNode(name, supertypeName, abstractKeyword == "abstract", properties, constructors)
    }
  }

  private def entity[_: P]: P[TypeDeclNode.EntityNode] = {
    def ownedBy = P(("owned by" ~ typeExpression).?)
    P("abstract".?.! ~ "entity" ~/ identifier ~ ownedBy ~ `extends` ~ classBody(property | component)).map {
      case (abstractKeyword, name, ownedBy, supertypeName, (members, constructors)) =>
        TypeDeclNode.EntityNode(name, supertypeName, ownedBy, abstractKeyword == "abstract", members, constructors)
    }
  }

  private def classBody[Member, _: P](member: => P[Member]): P[(List[Member], List[TypeDeclNode.ConstructorNode])] = {
    def members = P(member.repX(sep = Space.terminators)).map(_.toList)
    def constructors = P(constructor.repX(sep = Space.terminators)).map(_.toList)
    P("{" ~ members ~ constructors ~ "}")
  }
  private def `extends`[_: P]: P[Option[String]] = P(("extends" ~ identifier).?)
  private def property[_: P]: P[TypeDeclNode.PropertyNode] = P("mut".?.! ~ identifier ~ ":" ~ typeExpression).map {
    case (mutKeyword, name, tpe) => TypeDeclNode.PropertyNode(name, tpe, mutKeyword == "mut")
  }
  private def component[_: P]: P[TypeDeclNode.ComponentNode] = P("component" ~ identifier ~ ("overrides" ~ identifier).?).map(TypeDeclNode.ComponentNode.tupled)
  private def constructor[_: P]: P[TypeDeclNode.ConstructorNode] = {
    def constructorCall = P(("." ~ identifier).? ~ arguments).map(TypeDeclNode.ConstructorCallNode.tupled)
    def thisCall = P("this" ~ constructorCall)
    def superCall = P("super" ~ constructorCall)
    def constructCall = P("construct" ~ arguments ~ ("with" ~ superCall).?).map(TypeDeclNode.ConstructNode.tupled)
    def continuation = P(thisCall | constructCall)
    def statements = P(statement.repX(0, Space.terminators)).map(_.toList)
    P(identifier ~ parameters ~ "{" ~ statements ~ continuation ~ "}").map(TypeDeclNode.ConstructorNode.tupled)
  }
}
