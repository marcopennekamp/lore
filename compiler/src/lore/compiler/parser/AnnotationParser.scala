package lore.compiler.parser

import lore.compiler.syntax.DeclNode.TypeVariableNode
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * We will probably formalize annotations down the line, but for now this parser exists exclusively to parse the
  * annotation `@where`, as well as simple annotations such as `@bench` and `@bench_only`.
  *
  * Every annotation must be terminated by a newline, with the next non-blank line adhering to an exact indentation.
  * Hence, annotation whitespace must usually exclude newlines, except when individual sections are separated by
  * commas (such sections adhering to `wlgi`).
  *
  * An annotation parser guarantees that, after parsing the annotation, the offset will be placed on the next non-blank
  * line after the indentation. So for example, assume we're parsing the following expression:
  *
  * {{{
  * module A
  *   @root
  *   module B
  * }}}
  *
  * After parsing `@root`, the offset will be placed at the beginning of `module B`.
  *
  * To ensure that an annotation terminates correctly (with the correct follow-up indentation), an optional annotation
  * parser should usually be wrapped in `backtrack`.
  */
trait AnnotationParser {
  _: Parser with TypeParameterParser with TypeParser with IndentationParser with WhitespaceParser =>
  /**
    * Parses a simple `@name` annotation without any arguments.
    */
  def simpleAnnotation(annotationName: => Option[String], indentation: Int): Option[String] = {
    if (!character('@')) return None
    annotationName <& terminateAnnotation(indentation)
  }

  def whereAnnotation(indentation: Int): Option[Vector[TypeVariableNode]] = {
    if (!word("@where") || !ws()) return None
    // TODO (syntax): `wlgi` here to allow the first type parameter to start on a line after `@where`?
    //      @where
    //        A <: SomeComplicatedTypeWithLongNames[AnotherVeryLongName, LongNameOfCourse],
    //        B <: AnotherVeryLongName,
    //      func ...
    //  This kind of syntax is possible now because of significant indentation!
    val typeVariables = collectSepWlgi(character(','), indentation, allowTrailing = true)(simpleTypeParameter())
    typeVariables.takeMinSize(1) <& terminateAnnotation(indentation)
  }

  private def terminateAnnotation(indentation: Int): Boolean = ws() *> nli(indentation)
}
