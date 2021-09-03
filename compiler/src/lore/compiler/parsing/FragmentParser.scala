package lore.compiler.parsing

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
  val annotationParser = new AnnotationParser(nameParser)
  val expressionParser = new ExpressionParser(nameParser)
  val typeParser = new TypeParser(nameParser)
  val typeParameterParser = new TypeParameterParser(nameParser)

  import typeParser.typeExpression
  import nameParser._

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
    def definition = P(("=" ~ expressionParser.expression).?)
    P(functionCommons("func", typeParser.typing.map(Some(_)), definition))
      .map(withPosition(DeclNode.FunctionNode.from _))
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    def definition = P(expressionParser.block.?)
    P(functionCommons("act", Pass.map(_ => None), definition))
      .map(withPosition(DeclNode.FunctionNode.from _))
  }

  private def functionCommons[_: P](
    keyword: => P[Unit],
    typing: => P[Option[TypeExprNode]],
    definition: => P[Option[ExprNode]],
  ) = {
    def commons = P(Index ~~ keyword ~~ Space.WS1 ~/ name ~ parameters ~ typing)
    def annotationStyle = {
      P(annotationParser.where ~ commons ~ definition ~~ Index)
        .map { case (typeVariables, (startIndex, name, parameters, outputType), body, endIndex) => (startIndex, name, typeVariables, parameters, outputType, body, endIndex) }
    }
    def inlineStyle = {
      P(commons ~ inlineWhere.?.map(_.getOrElse(Vector.empty)) ~ definition ~~ Index)
        .map { case (startIndex, name, parameters, outputType, typeVariables, body, endIndex) => (startIndex, name, typeVariables, parameters, outputType, body, endIndex) }
    }
    P(annotationStyle | inlineStyle)
  }

  private def parameters[_: P]: P[Vector[DeclNode.ParameterNode]] = {
    def named = P(name ~ typeParser.typing).map { case (name, tpe) => (Option(name), tpe) }
    def unnamed = P(typeExpression).map(tpe => (Option.empty, tpe))
    def parameter = P(Index ~ (named | unnamed) ~ Index)
      .map { case (startIndex, (name, tpe), endIndex) => (startIndex, name, tpe, endIndex) }
      .map(withPosition(DeclNode.ParameterNode))
    P("(" ~ parameter.rep(sep = ",") ~ ")").map(_.toVector)
  }

  private def inlineWhere[_: P]: P[Vector[DeclNode.TypeVariableNode]] = {
    P("where" ~~ Space.WS1 ~ typeParameterParser.simpleParameter.rep(1, CharIn(",")).map(_.toVector)).map(_.toVector)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(`type` | `trait` | struct)

  private def `type`[_: P]: P[TypeDeclNode.AliasNode] = {
    P(Index ~ "type" ~~ Space.WS1 ~/ typeName ~~ Space.WS ~~ typeVariables(typeParameterParser.simpleParameter) ~ "=" ~ typeExpression ~ Index).map(withPosition(TypeDeclNode.AliasNode))
  }

  private def `trait`[_: P]: P[TypeDeclNode.TraitNode] = {
    P(Index ~ "trait" ~~ Space.WS1 ~/ typeName ~~ Space.WS ~~ typeVariables(typeParameterParser.traitParameter) ~ `extends` ~ Index).map(withPosition(TypeDeclNode.TraitNode))
  }

  private def `extends`[_: P]: P[Vector[TypeExprNode]] = {
    P(("extends" ~~ Space.WS1 ~ typeExpression.rep(1, CharIn(",")).map(_.toVector)).?).map(_.getOrElse(Vector.empty))
  }

  private def struct[_: P]: P[TypeDeclNode.StructNode] = {
    def conciseForm = P("(" ~ property.rep(sep = CharIn(",")).map(_.toVector) ~ ")" ~ `extends`)
      .map { case (properties, extended) => (extended, properties) }

    // Note that given the implicit struct body as it is now (newline..end), we cannot both allow newlines and trailing
    // commas in `extends` clauses. If we allow newlines, a trailing comma could lead the parser to make bad decisions:
    //
    //    struct Hello extends
    //        World,
    //        ExclamationMark,
    //      text: String
    //    end
    //
    // If we don't allow newlines, trailing commas are fine, BUT they wouldn't be useful anyway, and structs with many
    // extends clauses will become very messy. Hence, we have to disallow trailing commas, so that this becomes legal:
    //
    //    struct Hello extends
    //        World,
    //        ExclamationMark
    //      text: String
    //    end
    // TODO (new syntax): Another way would be to disallow trailing commas if a newline is used to delimit the extends
    //                    clause from the properties, but also to allow them if `do` is used as an optional delimiter.
    //                    The code example would then become:
    //    struct Hello extends
    //        World,
    //        ExclamationMark,
    //    do
    //      text: String
    //    end
    //                    This seems to be the best approach, considering that it's also visually clearer.
    def bodyForm = P(`extends` ~~ structBody)

    P(Index ~~ "struct" ~~ Space.WS1 ~~/ structName ~~ Space.WS ~~ typeVariables(typeParameterParser.structParameter) ~~ Space.WS ~~ (conciseForm | bodyForm) ~~ Index)
      .map { case (startIndex, nameNode, typeVariables, (extended, properties), endIndex) => (startIndex, nameNode, typeVariables, extended, properties, endIndex) }
      .map(withPosition(TypeDeclNode.StructNode))
  }

  private def structBody[_: P]: P[Vector[TypeDeclNode.PropertyNode]] = {
    // We have to parse the properties in a tiered structure because newline separators may only be used with repX,
    // otherwise the newline is consumed by the whitespace parser. We want properties separated by commas to contain
    // a comment between the property and the comma, so we have to apply the .rep parser there.
    def propertyLine = P(property.rep(sep = CharIn(","))).map(_.toVector)
    def properties = P(propertyLine.repX(sep = Space.terminators)).map(_.toVector.flatten)
    P(Space.terminators ~~ properties ~ "end")
  }

  private def property[_: P]: P[TypeDeclNode.PropertyNode] = {
    P(Index ~ ("open" ~~ Space.WS1).!.?.map(_.isDefined) ~ ("mut" ~~ Space.WS1).!.?.map(_.isDefined) ~ name ~ typeParser.typing ~ defaultValue.? ~ Index)
      .map { case (startIndex, isOpen, isMutable, name, tpe, defaultValue, endIndex) => (startIndex, name, tpe, isOpen, isMutable, defaultValue, endIndex) }
      .map(withPosition(TypeDeclNode.PropertyNode))
  }

  private def defaultValue[_: P]: P[ExprNode] = P("=" ~ expressionParser.expression)

  private def typeVariables[_: P](typeVariable: => P[DeclNode.TypeVariableNode]): P[Vector[DeclNode.TypeVariableNode]] = {
    P(("[" ~ typeVariable.rep(1, CharIn(",")).map(_.toVector) ~ "]").?).map(_.getOrElse(Vector.empty))
  }
}
