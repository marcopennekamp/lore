package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Fragment, Position}
import lore.compiler.syntax._

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
  lazy val parsed: Compilation[Vector[DeclNode]] = {
    fastparse.parse(fragment.input, fullFragment(_)) match {
      case Parsed.Failure(_, _, extra) =>
        val message = s"Parsing failure: ${extra.trace().aggregateMsg}"
        // TODO: Give the correct index to the parsing error.
        Compilation.fail(ParsingError(message, Position(fragment, 0)))
      case Parsed.Success(result, _) => result.compiled
    }
  }

  def fullFragment[_: P]: P[Vector[DeclNode]] = {
    // We repeat topDeclaration an arbitrary amount of times, terminated by Space.terminators. The repX in contrast
    // to rep does not take whitespace into account. We don't want it to consume the newline that needs to be consumed
    // by Space.terminators!
    P(Space.WL ~~ topDeclaration.repX(0, Space.terminators) ~~ Space.WL ~~ End).map(_.toVector)
  }

  private def topDeclaration[_: P]: P[DeclNode] = P(function | action | typeDeclaration)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def function[_: P]: P[DeclNode.FunctionNode] = {
    P(
      Index ~ "function" ~/ identifier ~ parameters ~ typeParser.typing ~ functionTypeVariables ~ ("=" ~ expression).?
    ).map(withIndex(DeclNode.FunctionNode(_, _, _, _, _, _)))
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    P(Index ~ "action" ~/ identifier ~ parameters ~ functionTypeVariables ~ block.?).map(withIndex(DeclNode.FunctionNode.fromAction _))
  }

  private def parameters[_: P]: P[Vector[DeclNode.ParameterNode]] = {
    def parameter = P(Index ~ identifier ~ typeParser.typing).map(withIndex(DeclNode.ParameterNode))
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toVector)
  }

  private def functionTypeVariables[_: P]: P[Vector[DeclNode.TypeVariableNode]] = {
    def typeVariable = {
      P(Index ~ identifier ~ (">:" ~ typeExpression).? ~ ("<:" ~ typeExpression).?).map(withIndex(DeclNode.TypeVariableNode))
    }
    P(("where" ~ typeVariable.rep(1, CharIn(","))).?).map {
      case None => Vector.empty
      case Some(seq) => seq.toVector
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(`trait` | struct)

  private def `trait`[_: P]: P[TypeDeclNode.TraitNode] = {
    P(Index ~ "independent".!.? ~ "trait" ~/ identifier ~ `extends` ~ ownedBy.?)
      .map { case (index, independent, name, (extended, components), ownedBy) => (index, name, extended, components, ownedBy, independent.isDefined) }
      .map(withIndex(TypeDeclNode.TraitNode))
  }

  private def `extends`[_: P]: P[(Vector[String], Vector[String])] = {
    // The boolean signifies whether the extended type is a component.
    def extendable = P("+".!.?.map(_.isDefined) ~ identifier)
    P(("extends" ~ extendable.rep(1, CharIn(","))).?).map {
      _.getOrElse(Seq.empty).foldLeft((Vector.empty[String], Vector.empty[String])) {
        case ((extended, components), extendable) => extendable match {
          case (false, name) => (extended :+ name, components)
          case (true, name) => (extended, components :+ name)
        }
      }
    }
  }

  private def struct[_: P]: P[TypeDeclNode.StructNode] = {
    P(Index ~ "independent".!.? ~ "struct" ~/ identifier ~ implements ~ ownedBy.? ~ structBody)
      .map { case (index, independent, name, implements, ownedBy, members) => (index, name, implements, ownedBy, members, independent.isDefined) }
      .map(withIndex(TypeDeclNode.StructNode))
  }

  private def implements[_: P]: P[Vector[String]] = {
    P(("implements" ~ identifier.rep(1, CharIn(","))).?)
      .map(_.map(_.toVector).getOrElse(Vector.empty))
  }

  private def structBody[_: P]: P[Vector[TypeDeclNode.MemberNode]] = {
    // We have to parse the members in a tiered structure because newline separators may only be used with repX,
    // otherwise the newline is consumed by the whitespace parser. We want members separated by commas to contain
    // a comment between the member and the comma, so we have to apply the .rep parser there.
    def member = P(property | component)
    def memberLine = P(member.rep(sep = CharIn(","))).map(_.toVector)
    def members = P(memberLine.repX(sep = Space.terminators)).map(_.toVector.flatten)
    P(("{" ~ members ~ "}").?).map(_.getOrElse(Vector.empty))
  }

  private def property[_: P]: P[TypeDeclNode.PropertyNode] = {
    P(Index ~ "mut".!.?.map(_.isDefined) ~ identifier ~ typeParser.typing ~ defaultValue.?)
      .map { case (index, isMutable, name, tpe, defaultValue) => (index, name, tpe, isMutable, defaultValue) }
      .map(withIndex(TypeDeclNode.PropertyNode))
  }

  private def component[_: P]: P[TypeDeclNode.ComponentNode] = {
    P(Index ~ "component" ~ identifier ~ defaultValue.?).map(withIndex(TypeDeclNode.ComponentNode))
  }

  private def defaultValue[_: P]: P[ExprNode] = P("=" ~ statementParser.expression)

  private def ownedBy[_: P]: P[TypeExprNode] = P("owned by" ~ typeExpression)
}
