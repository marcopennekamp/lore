package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, ParserFeedback}
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.TypeExprNode.TupleTypeNode
import lore.compiler.syntax._
import lore.compiler.types.AliasSchema.AliasVariant
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalaz.Scalaz.ToOptionIdOps

import scala.collection.mutable

// TODO (syntax): We should probably move annotation checking to the `constraints` and `resolution` phases.

trait DeclarationParser {
  _: Parser with AnnotationParser with TypeParameterParser with TypeParser with ExpressionParser with NameParser =>
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modules.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def moduleDeclaration(annotations: Vector[AnnotationNode]): Result[ModuleNode] = {
    val moduleKeyword = consume[TkModule].getOrElse {
      reporter.report(ParserFeedback.Declarations.DeclarationExpected("module".some, peek.position))
      return Failure
    }

    // TODO (syntax): "root" should be a constant somewhere. Maybe a sort of semantic registry of built-in annotations.
    val atRoot = annotations.exists {
      case SimpleAnnotationNode(name) => name.value == "root"
      case _ => false
    }
    checkAnnotationValidity(annotations) { annotation => annotation.uniqueName == "root" }.getOrElse(return Failure)

    val moduleName = namePath().getOrElse {
      // TODO (syntax): Report error: Module name required.
      return Failure
    }

    if (!consumeIf[TkNewline]) {
      // TODO (syntax): Report error.
      return Failure
    }

    val (imports, members) = withOptionalIndentation { isIndented =>
      if (isIndented) moduleDeclarationBody()
      else (Vector.empty, Vector.empty).success
    }.getOrElse(return Failure)

    val lastNode = members.lastOption.orElse(imports.lastOption).getOrElse(moduleName)
    ModuleNode(moduleName, atRoot, imports, members, moduleKeyword.position.to(lastNode)).success
  }

  def moduleDeclarationBody(): Result[(Vector[ImportNode], Vector[DeclNode])] = {
    val imports = collectSepLookaheadIs[Vector[ImportNode], TkUse](consumeIf[TkNewline]) {
      tkUse => moduleImport(tkUse)
    }.getOrElse(return Failure)

    // If the next token is a dedent, the module body is finished. Otherwise, there must be another module member.
    val members = collectSepLookahead(!peekIs[TkDedent], consumeIf[TkNewline]) {
      moduleMember()
    }.getOrElse(return Failure)

    (imports.flatten, members.flatten).success
  }

  private def moduleImport(tkUse: TkUse): Result[Vector[ImportNode]] = {
    val prefixPath = namePath().getOrElse {
      // TODO (syntax): Report error.
      return Failure
    }

    val isSpecialImport = consumeIf[TkDot]
    if (!isSpecialImport) {
      val position = tkUse.position.to(prefixPath)
      return Vector(DeclNode.ImportNode(prefixPath, isWildcard = false, position)).success
    }

    peek match {
      case underscore@TkUnderscore(_) =>
        skip()
        val position = tkUse.position.to(underscore.position)
        Vector(DeclNode.ImportNode(prefixPath, isWildcard = true, position)).success

      case TkBracketLeft(_) =>
        val (namePaths, listPosition) = bracketList(namePath()).getOrElse(return Failure)
        val position = tkUse.position.to(listPosition)
        namePaths.map { suffixPath =>
          // TODO (syntax): As noted in the TODO, this desugaring of list imports is wrong. Keep in mind that we will
          //                have to change the node here.
          val completeNamePath = NamePathNode(prefixPath.segments ++ suffixPath.segments)
          DeclNode.ImportNode(completeNamePath, isWildcard = false, position)
        }.success

      case _ =>
        // TODO (syntax): Report error: Expected list or wildcard import.
        Failure
    }
  }

  private def moduleMember(): Result[Vector[DeclNode]] = {
    val memberAnnotations = annotations().getOrElse(return Failure)

    // At this point, since annotations have been parsed, the next token must be the declaration keyword.
    moduleMemberSingle(memberAnnotations).backtrack.map(Vector(_)) | moduleMemberMulti(memberAnnotations)
  }

  private def moduleMemberSingle(annotations: Vector[AnnotationNode]): Result[DeclNode] = peek match {
    case TkModule(_) => moduleDeclaration(annotations)
    case TkType(_) => aliasDeclaration(annotations)
    case TkTrait(_) => traitDeclaration(annotations)

    case token: TkStruct =>
      structDeclaration(annotations) |
        aliasDeclaration(annotations) |
        failDeclarationExpected("struct or struct alias".some, token.position)

    case TkSpec(_) => specDeclaration(annotations)

    case token: TkObject =>
      objectDeclaration(annotations) |
        aliasDeclaration(annotations) |
        failDeclarationExpected("object or object alias".some, token.position)

    case TkLet(_) => globalVariableDeclaration(annotations)
    case TkFunc(_) => functionDeclaration(annotations)
    case TkProc(_) => procedureDeclaration(annotations)

    case _ => Failure // This failure will be backtracked to try `moduleMemberMulti`.
  }

  private def moduleMemberMulti(annotations: Vector[AnnotationNode]): Result[Vector[DeclNode]] = peek match {
    case TkDomain(_) => domainDeclaration(annotations)
    case token => failDeclarationExpected(None, token.position)
  }

  private def failDeclarationExpected(expectation: Option[String], position: Position): Result[Nothing] = {
    reporter.report(ParserFeedback.Declarations.DeclarationExpected(expectation, position))
    Failure
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // User-defined types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def aliasDeclaration(annotations: Vector[AnnotationNode]): Result[AliasNode] = {
    checkAnnotationsEmpty(annotations).getOrElse(return Failure)

    val startKeyword = consume()
    val variant = startKeyword match {
      case TkType(_) => AliasVariant.Type
      case TkStruct(_) => AliasVariant.Struct
      case TkObject(_) => AliasVariant.Object
      case _ =>
        // TODO (syntax): Report error: Expected type, struct, or object keyword for alias declaration.
        return Failure
    }

    val name = typeName().getOrElse {
      // TODO (syntax): Report error: Alias name expected.
      return Failure
    }
    val typeParameters = parseTypeParameters(simpleTypeParameter).discardPosition.getOrElse(return Failure)
    consumeExpect[TkEquals].getOrElse(return Failure)

    val bodyType = withOptionalIndentation(_ => typeExpression()).getOrElse(return Failure)
    AliasNode(name, variant, typeParameters, bodyType, startKeyword.position.to(bodyType)).success
  }

  private def traitDeclaration(annotations: Vector[AnnotationNode]): Result[TraitNode] = {
    checkAnnotationsEmpty(annotations).getOrElse(return Failure)

    val traitKeyword = consumeExpect[TkTrait].getOrElse(return Failure)
    val name = typeName().getOrElse {
      // TODO (syntax): Report error: Trait name expected.
      return Failure
    }
    val (typeParameters, typeParametersPosition) = parseTypeParameters(traitTypeParameter).getOrElse(return Failure)
    val extendedTypes = parseExtends().getOrElse(return Failure)

    val position = traitKeyword.position.toEither(extendedTypes.lastOption, typeParametersPosition, name.position)
    TraitNode(name, typeParameters, extendedTypes, position).success
  }

  private def structDeclaration(annotations: Vector[AnnotationNode]): Result[StructNode] = {
    checkAnnotationsEmpty(annotations).getOrElse(return Failure)

    Failure
  }

  private def objectDeclaration(annotations: Vector[AnnotationNode]): Result[StructNode] = {
    checkAnnotationsEmpty(annotations).getOrElse(return Failure)

    Failure
  }

//  private def extendsClause(): Option[Vector[TypeExprNode]] = {
//    // `extends` must be preceded by whitespace (and on the same line as the preceding token).
//    if (!ws() || !word("extends")) return None
//
//    // `extends` must either be followed by whitespace or a newline.
//    if (!ws() && !peekNewline()) return None
//
//    // TODO (syntax): The `indentation` in `typeExpression` should be equal to each line's `wlgi`, but instead is only
//    //                the minimum indentation of the extends clause. This could lead to syntax like this:
//    //   ```
//    //   module A
//    //     trait B extends
//    //         C[
//    //     String,
//    //     Int,
//    //     ]
//    //   ```
//    //   `String`, `Int`, and `]` should be indented at least as far as `C[`.
//    //   `wlmi` has the same problem as `wlgi`.
//    //   We should definitely replace some uses of `wlgi` (and maybe `wlmi`) with proper `indent` sections.
//    //   Alternatively, `wlgi` and `wlmi` could give the inner `indentation` as an argument to the `get` lambda.
//    //collectSepWlgi(character(','), indentation)(typeExpression(indentation)).takeNonEmpty
//    ???
//  }

  private def parseTypeParameters(
    parseTypeParameter: () => Result[TypeVariableNode],
  ): Result[(Vector[TypeVariableNode], Option[Position])] = {
    if (!peekIs[TkBracketLeft]) return (Vector.empty, None).success
    bracketList(parseTypeParameter()).havingMinSize(1).map {
      case (elements, position) => (elements, position.some)
    }
  }

  private def parseExtends(): Result[Vector[TypeExprNode]] = ???

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def globalVariableDeclaration(annotations: Vector[AnnotationNode]): Result[GlobalVariableNode] = {
    checkAnnotationsEmpty(annotations).getOrElse(return Failure)

    val letKeyword = consumeExpect[TkLet].getOrElse(return Failure)
    val variableName = name().getOrElse {
      // TODO (syntax): Report error: Global variable name expected.
      return Failure
    }
    val variableType = typing().getOrElse(return Failure)
    consumeExpect[TkEquals].getOrElse(return Failure)

    // TODO (syntax): We might have to open an optional block here in case there is an indentation...
    val value = parseExpression().getOrElse(return Failure)

    GlobalVariableNode(variableName, variableType, value, letKeyword.position.to(value)).success
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Functions and domains.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def functionDeclaration(annotations: Vector[AnnotationNode]): Result[FunctionNode] =
    functionLikeDeclaration(
      parseKeyword = () => consumeExpect[TkFunc],
      parseBodyMarker = () => consumeIf[TkEquals],
      None,
      annotations,
    )

  private def procedureDeclaration(annotations: Vector[AnnotationNode]): Result[FunctionNode] =
    functionLikeDeclaration(
      parseKeyword = () => consumeExpect[TkProc],
      parseBodyMarker = () => consumeIf[TkDo],
      Some(TupleTypeNode(Vector.empty, Position.internal)),
      annotations,
    )

  private def functionLikeDeclaration(
    parseKeyword: () => Result[Token],
    parseBodyMarker: () => Boolean,
    forcedReturnType: Option[TypeExprNode],
    annotations: Vector[AnnotationNode],
  ): Result[FunctionNode] = {
    checkAnnotationValidity(annotations) {
      case _: WhereAnnotationNode => true
      case _ => false
    }.getOrElse(return Failure)

    val maybeWhereAnnotation = annotations.findType[WhereAnnotationNode]

    val startKeyword = parseKeyword().getOrElse(return Failure)
    val functionName = name().getOrElse {
      // TODO (syntax): Report error: Function name expected.
      return Failure
    }
    val (parameters, parametersPosition) = parenList(functionParameter()).getOrElse(return Failure)
    val returnType = forcedReturnType.getOrElse {
      typing().getOrElse(return Failure)
    }
    val typeParameters = maybeWhereAnnotation.map(_.typeParameters).getOrElse {
      inlineWhere().backtrack.getOrElse(Vector.empty)
    }

    val hasBodyMarker = parseBodyMarker()
    val body = if (hasBodyMarker) {
      // TODO (syntax): We need to open a block right away for procs, but functions should be able to get away with an
      //                expression on the same line...
      parseExpression().getOrElse(return Failure).some
    } else None

    val position = startKeyword.position.toEither(
      body,
      typeParameters.lastOption,
      // We should only use the return type's position if it exists in the source code.
      if (forcedReturnType.isEmpty) returnType else parametersPosition,
    )
    FunctionNode(functionName, parameters, returnType, typeParameters, body, position).success
  }

  /**
    * Note that a domain's parameters may not have a trailing comma because a domain is terminated by a newline.
    */
  def domainDeclaration(annotations: Vector[AnnotationNode]): Result[Vector[FunctionNode]] = {
    checkAnnotationValidity(annotations) {
      case _: WhereAnnotationNode => true
      case _ => false
    }.getOrElse(return Failure)

    val maybeWhereAnnotation = annotations.find(_.isInstanceOf[WhereAnnotationNode])

    if (!word("domain") || !ws()) return None
    val domainParameters = ??? //collectSepWlgi(character(','), indentation)(functionParameter(indentation))
    ws()
    val domainTypeParameters = maybeWhereAnnotation match {
      case Some(typeParameters) => typeParameters
      case None => inlineWhere().backtrack.getOrElse(Vector.empty)
    }

    val bodyIndentation = indent(indentation).getOrElse(return None)
    val functions = collectSepBacktrack(nli(bodyIndentation)) {
      functionDeclaration(bodyIndentation).backtrack | procedureDeclaration(bodyIndentation)
    }

//    functions.map { function =>
//      FunctionNode(
//        function.nameNode,
//        //domainParameters ++ function.parameters,
//        function.outputType,
//        domainTypeParameters ++ function.typeVariables,
//        function.body,
//        function.position,
//      )
//    }.some
    ???
  }

  private def functionParameter(): Result[ParameterNode] = {
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

  private def inlineWhere(): Result[Vector[DeclNode.TypeVariableNode]] = {
    if (!consumeIf[TkWhere]) {
      // TODO (syntax): Report error.
      return Failure
    }

    // Because an inline where is supposed to be simple, we're disallowing trailing commas and newlines here.
    collectSepBacktrack(consumeIf[TkComma])(simpleTypeParameter()).success

    // TODO (syntax): Throw a `Failure` if no type parameters have been parsed.
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Specs.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def specDeclaration(annotations: Vector[AnnotationNode]): Result[SpecNode] = Failure

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def checkAnnotationsEmpty(annotations: Vector[AnnotationNode]): Result[Unit] = {
    if (annotations.nonEmpty) {
      reporter.report(annotations.map(ParserFeedback.Annotations.IllegalKind))
      return Failure
    }
    Success.empty
  }

  /**
    * Checks `annotations` for their kind with `isKindAllowed` and checks duplicates. Reports errors and returns
    * [[Failure]] if any checks failed.
    */
  private def checkAnnotationValidity(
    annotations: Vector[AnnotationNode],
  )(isKindAllowed: AnnotationNode => Boolean): Result[Unit] = {
    var errors = Vector.empty[Feedback]
    val uniqueNamesSeen = mutable.HashSet[String]()

    annotations.foreach { annotation =>
      if (!isKindAllowed(annotation)) {
        errors :+= ParserFeedback.Annotations.IllegalKind(annotation)
      }

      if (!annotation.areMultipleAllowed) {
        if (uniqueNamesSeen.contains(annotation.uniqueName)) {
          errors :+= ParserFeedback.Annotations.IllegalDuplicate(annotation)
        }
        uniqueNamesSeen.add(annotation.uniqueName)
      }
    }

    if (errors.isEmpty) Success.empty else Failure
  }
}
