package lore.compiler.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.syntax.DeclNode.TypeVariableNode
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
    P(Space.WL ~~ topDeclaration.repX(0, Space.terminators) ~~ Space.WL ~~ End).map(_.toVector.flatten)
  }

  private def topDeclaration[_: P]: P[Vector[DeclNode]] = {
    def single = P(function | action | typeDeclaration).map(Vector(_))
    P(single | domain)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Function declarations and domains.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def function[_: P]: P[DeclNode.FunctionNode] = {
    def definition = P((Space.WS ~~ "=" ~ expressionParser.expression).?)
    P(functionCommons("func", typeParser.typing.map(Some(_)), definition))
  }

  private def action[_: P]: P[DeclNode.FunctionNode] = {
    def definition = P((Space.WS ~~ expressionParser.block).?)
    P(functionCommons("act", Pass.map(_ => None), definition))
  }

  /**
    * Note that a domain's parameters may not have a trailing comma because a domain is terminated by a newline.
    */
  private def domain[_: P]: P[Vector[DeclNode.FunctionNode]] = {
    def parameters = P(parameter.rep(sep = ",")).map(_.toVector)
    def commons = P("domain" ~~ Space.WS1 ~~/ parameters)
    def functions = P((function | action).repX(1, Space.terminators)).map(_.toVector)
    def definition = P(Space.terminators ~~ functions ~ "end")

    P(whereStyles(commons, definition)).map { case (_, domainParameters, domainTypeVariables, functions, _) =>
      functions.map { function =>
        DeclNode.FunctionNode(
          function.nameNode,
          domainParameters ++ function.parameters,
          function.outputType,
          domainTypeVariables ++ function.typeVariables,
          function.body,
          function.position,
        )
      }
    }
  }

  private def functionCommons[_: P](
    keyword: => P[Unit],
    typing: => P[Option[TypeExprNode]],
    definition: => P[Option[ExprNode]],
  ): P[DeclNode.FunctionNode] = {
    def parameters = P("(" ~ parameter.rep(sep = ",") ~ ",".? ~ ")").map(_.toVector)
    def commons = P(keyword ~~ Space.WS1 ~~/ name ~ parameters ~ typing)
    P(whereStyles(commons, definition))
      .map { case (startIndex, (name, parameters, outputType), typeVariables, body, endIndex) => (startIndex, name, typeVariables, parameters, outputType, body, endIndex) }
      .map(withPosition(DeclNode.FunctionNode.from _))
  }

  def parameter[_: P]: P[DeclNode.ParameterNode] = {
    def named = P(name ~ typeParser.typing).map { case (name, tpe) => (Option(name), tpe) }
    def unnamed = P(typeExpression).map(tpe => (Option.empty, tpe))

    P(Index ~ (named | unnamed) ~ Index)
      .map { case (startIndex, (name, tpe), endIndex) => (startIndex, name, tpe, endIndex) }
      .map(withPosition(DeclNode.ParameterNode))
  }

  private def whereStyles[_: P, A, B](
    head: => P[A],
    definition: => P[B],
  ) = {
    def annotationStyle = {
      P(annotationParser.where ~ Index ~~ head ~~ definition ~~ Index)
        .map { case (typeVariables, startIndex, a, b, endIndex) => (startIndex, a, typeVariables, b, endIndex) }
    }

    // Because an inline where is supposed to be simple, we're disallowing trailing commas here.
    def typeParameters = P(typeParameterParser.simpleParameter.rep(1, CharIn(",")).map(_.toVector))
    def inlineWhere = P("where" ~~ Space.WS1 ~ typeParameters).map(_.toVector)
    def inlineStyle = P(Index ~~ head ~~ Space.WS ~~ inlineWhere.?.map(_.getOrElse(Vector.empty)) ~~ definition ~~ Index)

    P(annotationStyle | inlineStyle)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Type Declarations.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def typeDeclaration[_: P]: P[TypeDeclNode] = P(`type` | `trait` | struct | `object`)

  private def `type`[_: P]: P[TypeDeclNode.AliasNode] = {
    P(Index ~ "type" ~~ Space.WS1 ~/ typeName ~~ Space.WS ~~ typeVariables(typeParameterParser.simpleParameter) ~ "=" ~ typeExpression ~ Index).map(withPosition(TypeDeclNode.AliasNode))
  }

  private def `trait`[_: P]: P[TypeDeclNode.TraitNode] = {
    P(Index ~ "trait" ~~ Space.WS1 ~/ typeName ~~ Space.WS ~~ typeVariables(typeParameterParser.traitParameter) ~ `extends` ~ Index).map(withPosition(TypeDeclNode.TraitNode))
  }

  /**
    * Note that, because traits and structs may be terminated with a newline after the end of the `extends` list, we
    * cannot allow trailing commas here.
    */
  private def `extends`[_: P]: P[Vector[TypeExprNode]] = {
    P(("extends" ~~ Space.WS1 ~ typeExpression.rep(1, CharIn(",")).map(_.toVector)).?).map(_.getOrElse(Vector.empty))
  }

  private def struct[_: P]: P[TypeDeclNode.StructNode] = {
    structCommons("struct", typeVariables(typeParameterParser.structParameter), isObject = false)
  }

  private def `object`[_: P]: P[TypeDeclNode.StructNode] = {
    structCommons("object", Pass.map(_ => Vector.empty), isObject = true)
  }

  private def structCommons[_: P](
    keyword: => P[Unit],
    tvs: => P[Vector[TypeVariableNode]],
    isObject: Boolean,
  ): P[TypeDeclNode.StructNode] = {
    def conciseForm = {
      P("(" ~ property.rep(sep = CharIn(",")).map(_.toVector) ~ ",".? ~ ")" ~ `extends`)
        .map { case (properties, extended) => (extended, properties) }
    }

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

    def emptyForm = if (isObject) P(`extends`).map(extended => (extended, Vector.empty)) else Fail

    P(Index ~~ keyword ~~ Space.WS1 ~~/ structName ~~ Space.WS ~~ tvs ~~ Space.WS ~~ (conciseForm | bodyForm | emptyForm) ~~ Index)
      .map { case (startIndex, nameNode, typeVariables, (extended, properties), endIndex) => (startIndex, nameNode, isObject, typeVariables, extended, properties, endIndex) }
      .map(withPosition(TypeDeclNode.StructNode))
  }

  private def structBody[_: P]: P[Vector[TypeDeclNode.PropertyNode]] = {
    // Property lines should be terminated by newlines, so we're consciously not allowing trailing commas here.
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
    P(("[" ~ typeVariable.rep(1, CharIn(",")).map(_.toVector) ~ ",".? ~ "]").?).map(_.getOrElse(Vector.empty))
  }
}
