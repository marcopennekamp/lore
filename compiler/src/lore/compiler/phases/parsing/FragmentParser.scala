package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.ast._
import lore.compiler.Fragment
import lore.compiler.LoreCompiler.SourceFragment

/**
  * The parsers contained in this parser collection all parse top-level declarations that can occur in a
  * Lore file (fragment). These are those nodes declared in [[DeclNode]].
  */
object FragmentParser {
  import LexicalParser.identifier
  import Node.withIndex
  import StatementParser.{block, expression}
  import TypeParser.typeExpression

  /**
    * Attempts to parse the fragment, either returning an error message (left) or a list of DeclNodes.
    */
  def parse(source: SourceFragment): Either[String, Fragment] = {
    val input = ParserInput.fromString(source.code)
    fastparse.parse(input, fragment(_)) match {
      case Parsed.Failure(_, _, extra) => Left(s"Parsing failure: ${extra.trace().aggregateMsg}")
      case Parsed.Success(result, _) => Right(new Fragment(source.name, input, result))
    }
  }

  def fragment[_: P]: P[List[DeclNode]] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    P(Space.WL ~~ topDeclaration.repX(0, Space.terminators) ~~ Space.WL ~~ End).map(_.toList)
  }

  def topDeclaration[_: P]: P[DeclNode] = P(Index ~ (function | action | typeDeclaration)).map(withIndex(identity _))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def function[_: P]: P[DeclNode.FunctionNode] = {
    P(
      "function" ~/ identifier ~ parameters ~ TypeParser.typing ~ functionTypeVariables ~ ("=" ~ expression).?
    ).map((DeclNode.FunctionNode.apply _).tupled)
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    P("action" ~/ identifier ~ parameters ~ functionTypeVariables ~ block.?).map((DeclNode.FunctionNode.fromAction _).tupled)
  }

  private def parameters[_: P]: P[List[DeclNode.ParameterNode]] = {
    def parameter = P(Index ~ identifier ~ TypeParser.typing).map(withIndex(DeclNode.ParameterNode))
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toList)
  }

  private def functionTypeVariables[_: P]: P[List[DeclNode.TypeVariableNode]] = {
    def typeVariable = {
      P(Index ~ identifier ~ ("<:" ~ TypeParser.typeExpression).?).map(withIndex(DeclNode.TypeVariableNode))
    }
    P(("where" ~ typeVariable.rep(1)).?).map {
      case None => Nil
      case Some(seq) => seq.toList
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(alias | label | `class` | entity)

  private def alias[_: P]: P[TypeDeclNode.AliasNode] = P("type" ~/ identifier ~ "=" ~ typeExpression).map(TypeDeclNode.AliasNode.tupled)
  private def label[_: P]: P[TypeDeclNode.LabelNode] = P("label" ~/ identifier ~ `extends`).map(TypeDeclNode.LabelNode.tupled)

  // The only difference between classes and entities is that only entities can contain component declarations.
  // Once we have moved beyond the parsing stage, the compiler sees both classes and entities as ClassNodes, ClassTypes,
  // CLassDefinitions, etc.
  private def `class`[_: P]: P[TypeDeclNode.ClassNode] = P(dataType("class", property))
  private def entity[_: P]: P[TypeDeclNode.ClassNode] = P(dataType("entity", property | component))

  private def dataType[_: P](kind: => P[Unit], member: => P[TypeDeclNode.MemberNode]): P[TypeDeclNode.ClassNode] = {
    P("abstract".?.! ~ kind.! ~/ identifier ~ `extends` ~ ownedBy ~ classBody(member)).map {
      case (abstractKeyword, kind, name, supertypeName, ownedBy, (properties, constructors)) =>
        TypeDeclNode.ClassNode(name, supertypeName, ownedBy, abstractKeyword == "abstract", kind == "entity", properties, constructors)
    }
  }
  private def classBody[Member, _: P](member: => P[Member]): P[(List[Member], List[TypeDeclNode.ConstructorNode])] = {
    def members = P(member.repX(sep = Space.terminators)).map(_.toList)
    def constructors = P(constructor.repX(sep = Space.terminators)).map(_.toList)
    P(("{" ~ members ~ constructors ~ "}").?).map {
      case None => (List.empty, List.empty)
      case Some(x) => x
    }
  }
  private def ownedBy[_: P]: P[Option[TypeExprNode]] = P(("owned by" ~ typeExpression).?)
  private def `extends`[_: P]: P[Option[String]] = P(("extends" ~ identifier).?)
  private def property[_: P]: P[TypeDeclNode.PropertyNode] = P(Index ~ "mut".?.! ~ identifier ~ TypeParser.typing).map {
    case (index, mutKeyword, name, tpe) => withIndex(TypeDeclNode.PropertyNode)(index, name, tpe, mutKeyword == "mut")
  }
  private def component[_: P]: P[TypeDeclNode.ComponentNode] = {
    P(Index ~ "component" ~ identifier ~ ("overrides" ~ identifier).?).map(withIndex(TypeDeclNode.ComponentNode))
  }
  private def constructor[_: P]: P[TypeDeclNode.ConstructorNode] = {
    P(Index ~ identifier ~ parameters ~ block).map(withIndex(TypeDeclNode.ConstructorNode))
  }
}
