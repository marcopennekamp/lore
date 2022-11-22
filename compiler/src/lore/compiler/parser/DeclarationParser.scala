package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.TypeExprNode.TupleTypeNode
import lore.compiler.syntax.{DeclNode, ExprNode, TypeExprNode}
import scalaz.Scalaz.ToOptionIdOps

trait DeclarationParser { _: Parser with AnnotationParser with TypeParameterParser with TypeParser with BasicParsers =>
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modules.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def moduleDeclaration(indentation: Int): Option[ModuleNode] = {
    val startIndex = offset

    val atRoot = simpleAnnotation(word("root") &> Some("root"), indentation).backtrack.isDefined
    if (!word("module") || !ws()) return None

    val moduleName = namePath().getOrElse(return None)
    val (imports, members) = indent(indentation)
      .flatMap(bodyIndentation => moduleDeclarationBody(bodyIndentation))
      .getOrElse((Vector.empty, Vector.empty))

    ModuleNode(moduleName, atRoot, imports, members, createPositionFrom(startIndex)).some
  }

  // TODO (syntax): Parse imports.
  def moduleDeclarationBody(indentation: Int): Option[(Vector[ImportNode], Vector[DeclNode])] = {
    println(s"Module body indentation: $indentation")
    val members = collectSep(nli(indentation)) {
      // TODO (syntax): This optimization needs to be taken very carefully. For example, an `@root` module will start
      //                with `@`, not `m`. If any top-level declaration other than a module can start with the letter
      //                `m`, this must be changed. (For example by falling back on the default case if any of the
      //                optimistic cases fail.) Note that `proc` will suffer from this if we add a `private` keyword.
      peek match {
        case 'm' => moduleDeclaration(indentation)
        case 'l' => globalVariableDeclaration(indentation)
        case 'f' => functionDeclaration(indentation)
        case 'p' => procedureDeclaration(indentation)
        //case 't' => type alias or trait (differentiate by peek(2) == 'y' or 'r')
        //case 's' => struct or struct alias or spec (differentiate by peek(2) == 't' or 'p')
        //case 'o' => object or object alias
        case _ =>
          // All of these declarations may have annotations preceding their distinguishing keyword (`module`, `func`,
          // `proc`, and so on).
          moduleDeclaration(indentation).backtrack |
            globalVariableDeclaration(indentation).backtrack |
            functionDeclaration(indentation).backtrack |
            procedureDeclaration(indentation).backtrack
      }
    }
    (Vector.empty, members).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // User-defined types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def globalVariableDeclaration(indentation: Int): Option[GlobalVariableNode] = {
    val startIndex = offset
    if (!word("let") || !ws()) return None

    val variableName = name().getOrElse(return None)
    ws()
    val variableType = typing(indentation).getOrElse(return None)
    ws()

    if (!character('=') <* ws() || !word("TODO")) return None

    GlobalVariableNode(
      variableName,
      variableType,
      ExprNode.TupleNode(Vector.empty, Position.unknown),
      createPositionFrom(startIndex),
    ).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Functions and domains.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def functionDeclaration(indentation: Int): Option[FunctionNode] =
    functionLikeDeclaration(
      keyword ="func",
      None,
      bodyMarker = "=",
      indentation,
    )

  def procedureDeclaration(indentation: Int): Option[FunctionNode] = {
    val startIndex = offset
    functionLikeDeclaration(
      keyword = "proc",
      Some(TupleTypeNode(Vector.empty, createPositionFrom(startIndex))), // TODO (syntax): Not the most ideal position.
      bodyMarker = "do",
      indentation,
    )
  }

  private def functionLikeDeclaration(
    keyword: String,
    forcedReturnType: Option[TypeExprNode],
    bodyMarker: String,
    indentation: Int,
  ): Option[FunctionNode] = {
    val startIndex = offset

    val maybeWhereAnnotation = whereAnnotation(indentation).backtrack

    if (!word(keyword) || !ws()) return None
    val functionName = name().getOrElse(return None)
    ws()
    val parameters = functionParameterList(indentation).getOrElse(return None)
    ws()
    val returnType = forcedReturnType match {
      case Some(node) => node
      case None => typing(indentation).getOrElse(return None)
    }
    val typeParameters = maybeWhereAnnotation match {
      case Some(typeParameters) => typeParameters
      case None => (ws() &> inlineWhere()).backtrack.getOrElse(Vector.empty)
    }

    ws()
    if (!word(bodyMarker)) return None
    ws()
    if (!word("TODO")) return None

    FunctionNode(functionName, parameters, returnType, typeParameters, None, createPositionFrom(startIndex)).some
  }

  /**
    * Note that a domain's parameters may not have a trailing comma because a domain is terminated by a newline.
    */
  def domain(indentation: Int): Option[Vector[FunctionNode]] = {
    val maybeWhereAnnotation = whereAnnotation(indentation).backtrack

    if (!word("domain") || !ws()) return None
    val domainParameters = collectSepWlmi(character(','), indentation, allowTrailing = true)(functionParameter(indentation))
    ws()
    val domainTypeParameters = maybeWhereAnnotation match {
      case Some(typeParameters) => typeParameters
      case None => (ws() &> inlineWhere()).backtrack.getOrElse(Vector.empty)
    }

    val bodyIndentation = indent(indentation).getOrElse(return None)
    val functions = collectSep(nli(bodyIndentation)) {
      functionDeclaration(bodyIndentation).backtrack | procedureDeclaration(bodyIndentation)
    }

    functions.map { function =>
      FunctionNode(
        function.nameNode,
        domainParameters ++ function.parameters,
        function.outputType,
        domainTypeParameters ++ function.typeVariables,
        function.body,
        function.position,
      )
    }.some
  }

  private def functionParameter(indentation: Int): Option[ParameterNode] = {
    val startIndex = offset

    def named(): Option[ParameterNode] = for {
      parameterName <- name()
      parameterType <- typing(indentation)
    } yield ParameterNode(parameterName.some, parameterType, createPositionFrom(startIndex))

    def unnamed(): Option[ParameterNode] = typing(indentation).map {
      parameterType => ParameterNode(None, parameterType, createPositionFrom(startIndex))
    }

    named().backtrack orElse unnamed()
  }

  private def functionParameterList(indentation: Int): Option[Vector[ParameterNode]] = {
    if (!character('(')) return None
    wlmi(indentation)
    val parameters = collectSepWlmi(character(','), indentation, allowTrailing = true)(functionParameter(indentation))
    if (!character(')')) return None
    parameters.some
  }

  private def inlineWhere(): Option[Vector[DeclNode.TypeVariableNode]] = {
    if (!word("where") || !ws()) return None

    // Because an inline where is supposed to be simple, we're disallowing trailing commas and newlines here.
    collectSep(ws() *> character(',') <* ws())(simpleTypeParameter()).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Specs.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
