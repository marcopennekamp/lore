package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.syntax._

/**
  * The parsers contained in this parser collection all parse top-level declarations that can occur in a
  * fragment. These are those nodes declared in [[DeclNode]].
  */
class FragmentParser(implicit fragment: Fragment) {
  import Node._

  val nameParser = new NameParser
  import nameParser._

  val typeParser = new TypeParser(nameParser)
  import typeParser.typeExpression

  val expressionParser = new ExpressionParser(nameParser, typeParser)
  import expressionParser.{block, expression}

  case class ParsingError(fastparseError: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The file had parsing errors: $fastparseError"
  }

  /**
    * Attempts to parse the fragment and returns the parsing result.
    */
  def parse()(implicit reporter: Reporter): Vector[DeclNode] = {
    fastparse.parse(fragment.input, fullFragment(_)) match {
      case Parsed.Failure(_, _, extra) =>
        val error = ParsingError(s"Parsing failure: ${extra.trace().aggregateMsg}", Position(fragment, extra.index, extra.index))
        reporter.error(error)
        Vector.empty

      case Parsed.Success(result, _) => result
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
      Index ~ "function" ~~ Space.WS1 ~/ name ~ parameters ~ typeParser.typing ~ functionTypeVariables ~ ("=" ~ expression).? ~ Index
    ).map(withPosition(DeclNode.FunctionNode.fromFunction _))
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    P(Index ~ "action" ~~ Space.WS1 ~/ name ~ parameters ~ functionTypeVariables ~ block.? ~ Index).map(withPosition(DeclNode.FunctionNode.fromAction _))
  }

  private def parameters[_: P]: P[Vector[DeclNode.ParameterNode]] = {
    def parameter = P(Index ~ name ~ typeParser.typing ~ Index).map(withPosition(DeclNode.ParameterNode))
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toVector)
  }

  private def functionTypeVariables[_: P]: P[Vector[DeclNode.TypeVariableNode]] = {
    def typeVariable = {
      P(Index ~ typeName ~ (">:" ~ typeExpression).? ~ ("<:" ~ typeExpression).? ~ Index).map(withPosition(DeclNode.TypeVariableNode))
    }
    P(("where" ~~ Space.WS1 ~ typeVariable.rep(1, CharIn(","))).?).map {
      case None => Vector.empty
      case Some(seq) => seq.toVector
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(`type` | `trait` | struct)

  private def `type`[_: P]: P[TypeDeclNode.AliasNode] = {
    P(Index ~ "type" ~~ Space.WS1 ~/ typeName ~ "=" ~ typeExpression ~ Index).map(withPosition(TypeDeclNode.AliasNode))
  }

  private def `trait`[_: P]: P[TypeDeclNode.TraitNode] = {
    P(Index ~ "trait" ~~ Space.WS1 ~/ typeName ~ `extends` ~ Index).map(withPosition(TypeDeclNode.TraitNode))
  }

  private def struct[_: P]: P[TypeDeclNode.StructNode] = {
    P(Index ~ "struct" ~~ Space.WS1 ~/ structName ~ `extends` ~ structBody ~ Index).map(withPosition(TypeDeclNode.StructNode))
  }

  private def `extends`[_: P]: P[Vector[TypeExprNode]] = {
    P(("extends" ~~ Space.WS1 ~ inherits).?).map(_.getOrElse(Vector.empty))
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
    P(Index ~ ("open" ~~ Space.WS1).!.?.map(_.isDefined) ~ ("mut" ~~ Space.WS1).!.?.map(_.isDefined) ~ name ~ typeParser.typing ~ defaultValue.? ~ Index)
      .map { case (startIndex, isOpen, isMutable, name, tpe, defaultValue, endIndex) => (startIndex, name, tpe, isOpen, isMutable, defaultValue, endIndex) }
      .map(withPosition(TypeDeclNode.PropertyNode))
  }

  private def defaultValue[_: P]: P[ExprNode] = P("=" ~ expressionParser.expression)
}
