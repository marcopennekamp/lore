package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.TypeExprNode.TupleTypeNode
import lore.compiler.syntax.{DeclNode, ExprNode, TypeExprNode}
import lore.compiler.types.AliasSchema.AliasVariant
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

  def moduleDeclarationBody(indentation: Int): Option[(Vector[ImportNode], Vector[DeclNode])] = {
    println(s"Module body indentation: $indentation")

    val imports = collectSep(nli(indentation)) { moduleImport(indentation) }
    val members = collectSep(nli(indentation)) { moduleMember(indentation) }
    (imports.flatten, members).some
  }

  private def moduleImport(indentation: Int): Option[Vector[ImportNode]] = {
    val startIndex = offset

    if (!word("use") || !ws()) return None

    val prefixPath = namePath().getOrElse(return None)
    if (character('.')) {
      if (character('_')) {
        // Wildcard imports.
        Vector(DeclNode.ImportNode(prefixPath, isWildcard = true, createPositionFrom(startIndex))).some
      } else if (peek == '[') {
        // List imports.
        val namePaths = enclosedInBracketsWlmi(indentation, minSize = 1)(namePath()).getOrElse(return None)
        val position = createPositionFrom(startIndex)
        namePaths.map { suffixPath =>
          // TODO (syntax): As noted in the TODO, this desugaring of list imports is wrong. Keep in mind that we will
          //                have to change the node here.
          val completeNamePath = NamePathNode(prefixPath.segments ++ suffixPath.segments)
          DeclNode.ImportNode(completeNamePath, isWildcard = false, position)
        }.some
      } else None
    } else Vector(DeclNode.ImportNode(prefixPath, isWildcard = false, createPositionFrom(startIndex))).some
  }

  private def moduleMember(indentation: Int): Option[DeclNode] = {
    // TODO (syntax): The peek optimization needs to be taken carefully. For example, an `@root` module will start
    //                with `@`, not `m`. If any top-level declaration other than a module can start with the letter
    //                `m`, this must be changed. (For example by falling back on the default case if any of the
    //                optimistic cases fail.) Note that `proc` will suffer from this if we add a `private` keyword.
    //                Another issue is that annotations will be parsed many times before the correct declaration to
    //                parse is found. Perhaps we can pre-parse annotations (and any additional modifiers), and then
    //                later reject annotations/modifiers in the specific declaration parser.
    def default: Option[DeclNode] =
      moduleDeclaration(indentation).backtrack |
        aliasDeclaration(indentation).backtrack |
        traitDeclaration(indentation).backtrack |
        structDeclaration(indentation).backtrack |
        objectDeclaration(indentation).backtrack |
        globalVariableDeclaration(indentation).backtrack |
        functionDeclaration(indentation).backtrack |
        procedureDeclaration(indentation).backtrack |
        specDeclaration(indentation)

    peek match {
      case 'm' => moduleDeclaration(indentation)
      case 't' => peek(2) match {
        case 'y' => aliasDeclaration(indentation)
        case 'r' => traitDeclaration(indentation)
        case _ => default
      }
      case 's' => peek(2) match {
        case 't' => structDeclaration(indentation).backtrack | aliasDeclaration(indentation) // struct or struct alias
        case 'p' => specDeclaration(indentation)
        case _ => default
      }
      case 'o' => objectDeclaration(indentation).backtrack | aliasDeclaration(indentation) // object or object alias
      case 'l' => globalVariableDeclaration(indentation)
      case 'f' => functionDeclaration(indentation)
      case 'p' => procedureDeclaration(indentation)
      case _ => default
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // User-defined types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def aliasDeclaration(indentation: Int): Option[AliasNode] = {
    val startIndex = offset
    val aliasVariant =
      if (word("type")) AliasVariant.Type
      else if (word("struct")) AliasVariant.Struct
      else if (word("object")) AliasVariant.Object
      else return None

    if (!ws()) return None
    val aliasName = typeName().getOrElse(return None)
    ws()
    val typeParameters =
      enclosedInBracketsWlmi(indentation, minSize = 1)(simpleTypeParameter()).backtrack.getOrElse(Vector.empty)
    ws()
    if (!character('=')) return None

    // The type expression may be defined in an indentation block, or otherwise on the same line.
    val bodyIndentation = indentOrWs(indentation)
    val aliasType = typeExpression(bodyIndentation).getOrElse(return None)

    AliasNode(aliasName, aliasVariant, typeParameters, aliasType, createPositionFrom(startIndex)).some
  }

  private def traitDeclaration(indentation: Int): Option[TraitNode] = None

  private def structDeclaration(indentation: Int): Option[StructNode] = None

  private def objectDeclaration(indentation: Int): Option[StructNode] = None

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def globalVariableDeclaration(indentation: Int): Option[GlobalVariableNode] = {
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
  private def functionDeclaration(indentation: Int): Option[FunctionNode] =
    functionLikeDeclaration(
      keyword ="func",
      None,
      bodyMarker = "=",
      indentation,
    )

  private def procedureDeclaration(indentation: Int): Option[FunctionNode] = {
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
    val domainParameters = collectSepWlmi(character(','), indentation)(functionParameter(indentation))
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

  private def functionParameterList(indentation: Int): Option[Vector[ParameterNode]] =
    enclosedInParenthesesWlmi(indentation) { functionParameter(indentation) }

  private def inlineWhere(): Option[Vector[DeclNode.TypeVariableNode]] = {
    if (!word("where") || !ws()) return None

    // Because an inline where is supposed to be simple, we're disallowing trailing commas and newlines here.
    collectSep(ws() *> character(',') <* ws())(simpleTypeParameter()).some
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Specs.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def specDeclaration(indentation: Int): Option[SpecNode] = None
}
