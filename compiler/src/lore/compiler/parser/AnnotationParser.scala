package lore.compiler.parser

import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.syntax.{TkAnnotation, TkComma, TkNewline}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * We will probably formalize annotations down the line, but for now this parser exists exclusively to parse the
  * annotation `@where`, as well as simple annotations such as `@bench` and `@bench_only`.
  *
  * Every annotation must be terminated by a newline, without any following indent or dedent. Hence, annotation
  * tokens must usually be placed on the same line, except when individual sections are separated by commas.
  *
  * To ensure that an annotation terminates correctly (with the correct follow-up indentation), an optional annotation
  * parser should usually be wrapped in `backtrack`.
  */
trait AnnotationParser {
  _: Parser with TypeParameterParser with TypeParser with IndentationParser with ControlParser =>
  /**
    * Parses a simple `@name` annotation without any arguments.
    */
  def simpleAnnotation(name: String): Boolean = annotationHead(name) && terminateSingleLineAnnotation()

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
  def whereAnnotation(): Option[Vector[TypeVariableNode]] = {
    if (!annotationHead("where")) return None

    annotationBodyWithOptionalIndentation { isIndented =>
      collectSep(separatorNl(TkComma, allowNewline = isIndented), consumeOnly(TkComma)) { simpleTypeParameter() }
        .takeNonEmpty
    }
  }

  private def annotationHead(name: String): Boolean = {
    val annotationToken = consumeOnly[TkAnnotation]().getOrElse(return false)
    annotationToken.name == name
  }

  /**
    * Parses an annotation body in a [[withOptionalIndentation]] block and ensures that it's properly terminated,
    * including the [[TkNewline]] after `body` and that the next line has the same indentation.
    *
    * The boolean passed to `body` signifies whether an indentation section has been opened.
    */
  private def annotationBodyWithOptionalIndentation[A](body: Boolean => Option[A]): Option[A] = {
    val result = withOptionalIndentation { isIndented =>
      body(isIndented) <& consumeOnly(TkNewline)
    }
    result.flatten <& !peek.isControlToken
  }

  private def terminateSingleLineAnnotation(): Boolean = consumeOnly(TkNewline) && !peek.isControlToken
}
