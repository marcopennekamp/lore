package lore.lsp

import lore.compiler.core.Position
import lore.compiler.feedback.Feedback
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Range}
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j

import scala.jdk.CollectionConverters._

object FeedbackPublisher {

  def publish(feedback: Vector[Feedback])(implicit client: LanguageClient): Unit = {
    val byFragment = feedback.groupBy(_.position.fragment)
    byFragment.foreach { case (fragment, feedback) =>
      // Only publish diagnostics for fragments that have a path!
      fragment.path.foreach { path =>
        val diagnostics = feedback.map(toDiagnostic).asJava
        val uri = path.toUri.toString
        MessageLogger.info(s"Fragment URI: $uri")
        client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics))
      }
    }
  }

  private def toDiagnostic(feedback: Feedback): Diagnostic = feedback match {
    case _: Feedback.Warning => toDiagnostic(feedback, DiagnosticSeverity.Warning)
    case _: Feedback.Error => toDiagnostic(feedback, DiagnosticSeverity.Error)
  }

  private def toDiagnostic(feedback: Feedback, severity: DiagnosticSeverity): Diagnostic = {
    new Diagnostic(toRange(feedback.position), feedback.message, severity, "lore")
  }

  private def toRange(position: Position): Range = {
    val lspPosition = new lsp4j.Position(position.line - 1, position.column - 1)
    new Range(lspPosition, lspPosition)
  }

}
