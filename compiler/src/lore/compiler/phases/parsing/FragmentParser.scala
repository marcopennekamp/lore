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
  import LexicalParser.{identifier, structIdentifier, typeIdentifier}
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
      P(Index ~ typeIdentifier ~ (">:" ~ typeExpression).? ~ ("<:" ~ typeExpression).?).map(withIndex(DeclNode.TypeVariableNode))
    }
    P(("where" ~ typeVariable.rep(1, CharIn(","))).?).map {
      case None => Vector.empty
      case Some(seq) => seq.toVector
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(`type` | `trait` | struct)

  private def `type`[_: P]: P[TypeDeclNode.AliasNode] = {
    P(Index ~ "type" ~/ typeIdentifier ~ "=" ~ typeExpression).map(withIndex(TypeDeclNode.AliasNode))
  }

  private def `trait`[_: P]: P[TypeDeclNode.TraitNode] = {
    P(Index ~ "trait" ~/ typeIdentifier ~ `extends`).map(withIndex(TypeDeclNode.TraitNode))
  }

  private def `extends`[_: P]: P[Vector[TypeExprNode]] = {
    P(("extends" ~ inherits).?).map(_.getOrElse(Vector.empty))
  }

  private def struct[_: P]: P[TypeDeclNode.StructNode] = {
    P(Index ~ "struct" ~/ structIdentifier ~ implements ~ structBody).map(withIndex(TypeDeclNode.StructNode))
  }

  private def implements[_: P]: P[Vector[TypeExprNode]] = {
    P(("implements" ~ inherits).?).map(_.getOrElse(Vector.empty))
  }

  private def inherits[_: P]: P[Vector[TypeExprNode]] = P(typeExpression.rep(1, CharIn(","))).map(_.toVector)

  private def structBody[_: P]: P[Vector[TypeDeclNode.PropertyNode]] = {
    // We have to parse the properties in a tiered structure because newline separators may only be used with repX,
    // otherwise the newline is consumed by the whitespace parser. We want properties separated by commas to contain
    // a comment between the property and the comma, so we have to apply the .rep parser there.
    def propertyLine = P(property.rep(sep = CharIn(","))).map(_.toVector)
    def properties = P(propertyLine.repX(sep = Space.terminators)).map(_.toVector.flatten)
    P(("{" ~ properties ~ "}").?).map(_.getOrElse(Vector.empty))
  }

  private def property[_: P]: P[TypeDeclNode.PropertyNode] = {
    P(Index ~ "mut".!.?.map(_.isDefined) ~ identifier ~ typeParser.typing ~ defaultValue.?)
      .map { case (index, isMutable, name, tpe, defaultValue) => (index, name, tpe, isMutable, defaultValue) }
      .map(withIndex(TypeDeclNode.PropertyNode))
  }

  private def defaultValue[_: P]: P[ExprNode] = P("=" ~ statementParser.expression)
}
