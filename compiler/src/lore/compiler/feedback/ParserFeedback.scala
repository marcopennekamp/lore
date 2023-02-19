package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.syntax.{AnnotationNode, Token}

import scala.reflect.ClassTag

object ParserFeedback {

  case class TokenExpected[T <: Token](
    override val position: Position,
  )(implicit tag: ClassTag[T]) extends Feedback.Error(position) {
    // TODO (syntax): Map token class name to token description.
    override def message: String = s"Expected a ${tag.getClass.getSimpleName}."
  }

  object Declarations {
    case class DeclarationExpected(
      expectation: Option[String],
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = expectation match {
        case Some(expectation) => s"Expected a $expectation declaration."
        case None => "Expected a declaration."
      }
    }
  }

  object Annotations {
    case class IllegalKind(annotation: AnnotationNode) extends Feedback.Error(annotation.position) {
      override def message: String = s"This declaration may not have a `${annotation.uniqueName}` annotation."
    }

    case class IllegalDuplicate(annotation: AnnotationNode) extends Feedback.Error(annotation.position) {
      override def message: String = s"`${annotation.uniqueName}` is already defined for this declaration."
    }
  }

}
