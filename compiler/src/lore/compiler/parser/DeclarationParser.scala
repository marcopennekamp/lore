package lore.compiler.parser

import lore.compiler.core.Position
import lore.compiler.feedback.ParserFeedback
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.Node.NamePathNode
import lore.compiler.syntax.TypeExprNode.TupleTypeNode
import lore.compiler.syntax._
import lore.compiler.types.AliasSchema.AliasVariant
import scalaz.Scalaz.ToOptionIdOps

import scala.collection.mutable

// TODO (syntax): We should probably move annotation checking to the `constraints` and `resolution` phases.

trait DeclarationParser { _: Parser with AnnotationParser with TypeParameterParser with TypeParser with BasicParsers =>
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Modules.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def moduleDeclaration(annotations: Vector[AnnotationNode]): Result[ModuleNode] = {
    val moduleKeyword = consumeOnly[TkModule].getOrElse {
      reporter.report(ParserFeedback.Declarations.DeclarationExpected("module".some, peek.position))
      return Recoverable
    }

    // TODO (syntax): "root" should be a constant somewhere. Maybe a sort of semantic registry of built-in annotations.
    val atRoot = annotations.exists {
      case SimpleAnnotationNode(name) => name.value == "root"
      case _ => false
    }
    val areAnnotationsValid = checkAnnotationValidity(annotations) {
      annotation => annotation.uniqueName == "root"
    }
    if (!areAnnotationsValid) {
      return Failure // An error will already have been reported.
    }

    val moduleName = namePath().getOrElse {
      // TODO (syntax): Report error: Module name required.
      return Failure
    }

    val (imports, members) = indent()
      .flatMap(bodyIndentation => moduleDeclarationBody(bodyIndentation))
      .getOrElse((Vector.empty, Vector.empty))

    val lastNode = members.lastOption.orElse(imports.lastOption).getOrElse(moduleName)
    ModuleNode(moduleName, atRoot, imports, members, moduleKeyword.position.to(lastNode.position)).success
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

  private def moduleMember(): Result[DeclNode] = {
    val memberAnnotations = annotations().getOrElse(return Failure)

    def declarationExpected(expectation: Option[String], position: Position): Result[Nothing] = {
      reporter.report(ParserFeedback.Declarations.DeclarationExpected(expectation, position))
      Failure
    }

    // At this point, since annotations have been parsed, the next token must be the declaration keyword.
    peek match {
      case TkModule(_) => moduleDeclaration(memberAnnotations)
      case TkType(_) => aliasDeclaration(memberAnnotations)
      case TkTrait(_) => traitDeclaration(memberAnnotations)

      case token: TkStruct =>
        structDeclaration(memberAnnotations) |
          aliasDeclaration(memberAnnotations) |
          declarationExpected("struct or struct alias".some, token.position)

      case TkSpec(_) => specDeclaration(memberAnnotations)

      case token: TkObject =>
        objectDeclaration(memberAnnotations) |
          aliasDeclaration(memberAnnotations) |
          declarationExpected("object or object alias".some, token.position)

      case TkLet(_) => globalVariableDeclaration(memberAnnotations)
      case TkFunc(_) => functionDeclaration(memberAnnotations)
      case TkProc(_) => procedureDeclaration(memberAnnotations)
      case TkDomain(_) => domainDeclaration(memberAnnotations)

      case token if memberAnnotations.nonEmpty =>
        // If annotations have been parsed, there MUST be a declaration. If no annotations have been parsed, this might
        // be the end of the module body instead of another declaration, and thus the parse run is recoverable.
        if (memberAnnotations.nonEmpty) declarationExpected(None, token.position)
        else Recoverable
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // User-defined types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def aliasDeclaration(annotations: Vector[AnnotationNode]): Result[AliasNode] = {




    // TODO (syntax): Only check annotations once it's been established that this is non-recoverable.
    if (annotations.nonEmpty) {
      reportAnnotationsNotAllowed(annotations, "type alias")
      return Failure
    }

    return Recoverable

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

  private def traitDeclaration(annotations: Vector[AnnotationNode]): Result[TraitNode] = {
    // ...

    // TODO (syntax): Only check annotations once it's been established that this is non-recoverable.
    if (annotations.nonEmpty) {
      reportAnnotationsNotAllowed(annotations, "trait")
      return Failure
    }

  private def structDeclaration(indentation: Int): Option[StructNode] = None

  private def objectDeclaration(indentation: Int): Option[StructNode] = None

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Global variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def globalVariableDeclaration(annotations: Vector[AnnotationNode]): Result[GlobalVariableNode] = {
    if (annotations.nonEmpty) {
      reportAnnotationsNotAllowed(annotations, "global variable")
      return Failure
    }

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
  private def functionDeclaration(annotations: Vector[AnnotationNode]): Result[FunctionNode] =
    functionLikeDeclaration(
      keyword ="func",
      None,
      bodyMarker = "=",
      indentation,
    )

  private def procedureDeclaration(annotations: Vector[AnnotationNode]): Result[FunctionNode] = {
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
  def domainDeclaration(annotations: Vector[AnnotationNode]): Result[Vector[FunctionNode]] = {
    val maybeWhereAnnotation = whereAnnotation(indentation).backtrack

    if (!word("domain") || !ws()) return None
    val domainParameters = ??? //collectSepWlgi(character(','), indentation)(functionParameter(indentation))
    ws()
    val domainTypeParameters = maybeWhereAnnotation match {
      case Some(typeParameters) => typeParameters
      case None => (ws() &> inlineWhere()).backtrack.getOrElse(Vector.empty)
    }

    val bodyIndentation = indent(indentation).getOrElse(return None)
    val functions = collectSep(nli(bodyIndentation)) {
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
  private def specDeclaration(annotations: Vector[AnnotationNode]): Result[SpecNode] = Recoverable

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Helpers.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def reportAnnotationsNotAllowed(annotations: Vector[AnnotationNode]): Unit = {
    annotations.foreach { annotation =>
      reporter.report(ParserFeedback.Annotations.IllegalKind(annotation))
    }
  }

  /**
    * Checks `annotations` for their kind with `isKindAllowed` and reports duplicates. Returns whether all annotations
    * are valid.
    */
  private def checkAnnotationValidity(
    annotations: Vector[AnnotationNode],
  )(isKindAllowed: AnnotationNode => Boolean): Boolean = {
    var isValid = true
    val uniqueNamesSeen = mutable.HashSet[String]()

    annotations.foreach { annotation =>
      if (!isKindAllowed(annotation)) {
        isValid = false
        reporter.report(ParserFeedback.Annotations.IllegalKind(annotation))
      }

      if (!annotation.areMultipleAllowed) {
        if (uniqueNamesSeen.contains(annotation.uniqueName)) {
          isValid = false
          reporter.report(ParserFeedback.Annotations.IllegalDuplicate(annotation))
        }
        uniqueNamesSeen.add(annotation.uniqueName)
      }
    }

    isValid
  }
}
