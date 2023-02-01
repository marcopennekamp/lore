package lore.compiler.parser

import lore.compiler.syntax.Node.NameNode
import lore.compiler.syntax._

/**
  * [[AnnotationParser]] parses annotations into [[AnnotationNode]]s.
  *
  * Every annotation must be terminated by a newline, without any following indent or dedent. Hence, an annotation body
  * must usually be placed on the same line, except when the body is in an indentation section (for select annotations).
  */
trait AnnotationParser { _: Parser with TypeParameterParser with TypeParser with IndentationParser with ControlParser =>
  @StateConservative
  def annotations(): UnrecoverableResult[Vector[AnnotationNode]] = collect(annotation())

  private def annotation(): Result[AnnotationNode] = {
    val annotationHead = consumeOnly[TkAnnotation].getOrElse(return Recoverable)

    val result = annotationHead.name match {
      case "where" => whereAnnotation(annotationHead)
      case name => annotationBodyOnSingleLine { SimpleAnnotationNode(NameNode(name, annotationHead.position)).success }
    }

    // The newline terminator is checked by the specific annotation parsers, but we additionally have to make sure that
    // the next token (another annotation or the annotated element) has the same indentation.
    if (!peek.isControlToken) {
      // TODO (syntax): Report error. (Same indentation expected on the next line.)
      return Failure
    }

    result
  }

  /**
    * Parses a `@where` annotation. The type parameters may be provided on the same line as the `@where` or in an
    * indentation section, for example:
    *
    * {{{
    * @where
    *   A <: SomeComplicatedTypeWithLongNames[AnotherVeryLongName, LongNameOfCourse],
    *   B <: AnotherVeryLongName,
    * func ...
    * }}}
    */
  def whereAnnotation(annotationHead: TkAnnotation): UnrecoverableResult[WhereAnnotationNode] = {
    annotationBodyWithOptionalIndentation { isIndented =>
      val typeParameters = collectSep(separatorNl(consumeIf[TkComma], allowNewline = isIndented), consumeIf[TkComma]) {
        simpleTypeParameter()
      }

      if (typeParameters.isEmpty) {
        // TODO (syntax): Report error.
        return Failure
      }

      WhereAnnotationNode(typeParameters, annotationHead.position.to(typeParameters.last.position)).success
    }
  }

  /**
    * Parses a single-line annotation body and ensures that the annotation is terminated by a newline.
    */
  private def annotationBodyOnSingleLine[A, R >: UnrecoverableResult[A] <: Result[A]](body: => R): R = {
    val result = body
    if (!result.isSuccess) return result

    if (!consumeIf[TkNewline]) {
      // TODO (syntax): Report error. (Newline expected at end of annotation.)
      return Failure
    }

    result
  }

  /**
    * Parses an annotation body with optional indentation and ensures that it's terminated by a newline.
    *
    * The boolean passed to `body` signifies whether an indentation section has been opened.
    */
  private def annotationBodyWithOptionalIndentation[A, R >: UnrecoverableResult[A] <: Result[A]](body: Boolean => R): R = {
    val isIndented = openOptionalIndentation()
    val result = body(isIndented)

    if (!consumeIf[TkNewline]) {
      // TODO (syntax): Report error. (Newline expected at end of annotation.)
      return Failure
    }

    if (isIndented && !closeIndentation()) {
      return Failure // An error was already reported.
    }

    result
  }
}
