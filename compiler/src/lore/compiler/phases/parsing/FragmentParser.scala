package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.ast._
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, Fragment}
import lore.compiler.core.feedback.{Error, Position}

/**
  * The parsers contained in this parser collection all parse top-level declarations that can occur in a
  * fragment. These are those nodes declared in [[DeclNode]].
  */
class FragmentParser(implicit fragment: Fragment) {
  import LexicalParser.identifier
  import Node._

  val typeParser = new TypeParser
  import typeParser.typeExpression

  val statementParser = new StatementParser(typeParser)
  import statementParser.{block, expression}

  case class ParsingError(fastparseError: String, pos: Position) extends Error(pos) {
    override def message: String = s"The file had parsing errors: $fastparseError"
  }

  /**
    * Attempts to parse the fragment and returns the parsing result.
    */
  lazy val parsed: C[List[DeclNode]] = {
    fastparse.parse(fragment.input, fullFragment(_)) match {
      case Parsed.Failure(_, _, extra) =>
        val message = s"Parsing failure: ${extra.trace().aggregateMsg}"
        // TODO: Give the correct index to the parsing error.
        Compilation.fail(ParsingError(message, Position(fragment, 0)))
      case Parsed.Success(result, _) => Compilation.succeed(result)
    }
  }

  def fullFragment[_: P]: P[List[DeclNode]] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    P(Space.WL ~~ topDeclaration.repX(0, Space.terminators) ~~ Space.WL ~~ End).map(_.toList)
  }

  private def topDeclaration[_: P]: P[DeclNode] = P(function | action | typeDeclaration)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def function[_: P]: P[DeclNode.FunctionNode] = {
    P(
      Index ~ "function" ~/ identifier ~ parameters ~ typeParser.typing ~ functionTypeVariables ~ ("=" ~ expression).?
    ).map(withIndex(DeclNode.FunctionNode(_, _, _, _, _)))
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    P(Index ~ "action" ~/ identifier ~ parameters ~ functionTypeVariables ~ block.?).map(withIndex(DeclNode.FunctionNode.fromAction _))
  }

  private def parameters[_: P]: P[List[DeclNode.ParameterNode]] = {
    def parameter = P(Index ~ identifier ~ typeParser.typing).map(withIndex(DeclNode.ParameterNode(_, _)))
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toList)
  }

  private def functionTypeVariables[_: P]: P[List[DeclNode.TypeVariableNode]] = {
    def typeVariable = {
      P(Index ~ identifier ~ (">:" ~ typeParser.typeExpression).? ~ ("<:" ~ typeParser.typeExpression).?).map(withIndex(DeclNode.TypeVariableNode(_, _, _)))
    }
    P(("where" ~ typeVariable.rep(1, CharIn(","))).?).map {
      case None => Nil
      case Some(seq) => seq.toList
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(alias | label | `class` | entity)

  private def alias[_: P]: P[TypeDeclNode.AliasNode] = P(Index ~ "type" ~/ identifier ~ "=" ~ typeExpression).map(withIndex(TypeDeclNode.AliasNode(_, _)))
  private def label[_: P]: P[TypeDeclNode.LabelNode] = P(Index ~ "label" ~/ identifier ~ `extends`).map(withIndex(TypeDeclNode.LabelNode(_, _)))

  // The only difference between classes and entities is that only entities can contain component declarations.
  // Once we have moved beyond the parsing stage, the compiler sees both classes and entities as ClassNodes, ClassTypes,
  // CLassDefinitions, etc.
  private def `class`[_: P]: P[TypeDeclNode.ClassNode] = P(dataType("class", property))
  private def entity[_: P]: P[TypeDeclNode.ClassNode] = P(dataType("entity", property | component))

  private def dataType[_: P](kind: => P[Unit], member: => P[TypeDeclNode.MemberNode]): P[TypeDeclNode.ClassNode] = {
    P(Index ~ "abstract".?.! ~ kind.! ~/ identifier ~ `extends` ~ ownedBy ~ classBody(member)).map {
      case (index, abstractKeyword, kind, name, supertypeName, ownedBy, (properties, constructors)) =>
        attachIndex(TypeDeclNode.ClassNode(name, supertypeName, ownedBy, abstractKeyword == "abstract", kind == "entity", properties, constructors))(index)
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
  private def property[_: P]: P[TypeDeclNode.PropertyNode] = {
    P(Index ~ "mut".?.! ~ identifier ~ typeParser.typing)
      .map { case (index, mutKeyword, name, tpe) => (index, name, tpe, mutKeyword == "mut") }
      .map(withIndex(TypeDeclNode.PropertyNode(_, _, _)))
  }
  private def component[_: P]: P[TypeDeclNode.ComponentNode] = {
    P(Index ~ "component" ~ identifier ~ ("overrides" ~ identifier).?).map(withIndex(TypeDeclNode.ComponentNode(_, _)))
  }
  private def constructor[_: P]: P[TypeDeclNode.ConstructorNode] = {
    P(Index ~ identifier ~ parameters ~ block).map(withIndex(TypeDeclNode.ConstructorNode(_, _, _)))
  }
}
