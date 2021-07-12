package lore.lsp

import lore.compiler.core.{Fragment, Position}
import lore.compiler.feedback.Feedback
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Range}
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j

import scala.jdk.CollectionConverters._

class FeedbackPublisher {

  /**
    * We need to remember the fragments that last received feedback so that we can clear diagnostics from the file if
    * the fragment doesn't have feedback the next time.
    */
  var lastFeedbackFragments: Vector[Fragment] = Vector.empty

  def publish(feedback: Vector[Feedback])(implicit client: LanguageClient): Unit = this.synchronized {
    val byFragment = feedback.groupBy(_.position.fragment)

    // Make sure that fragment diagnostics are cleared for any fragments that don't have feedback.
    lastFeedbackFragments
      .filter(fragment => !byFragment.contains(fragment))
      .foreach(fragment => publish(fragment, Vector.empty))

    byFragment.foreach { case (fragment, feedback) => publish(fragment, feedback) }

    lastFeedbackFragments = byFragment.keys.toVector
  }

  private def publish(fragment: Fragment, feedback: Vector[Feedback])(implicit client: LanguageClient): Unit = {
    // Only publish diagnostics for fragments that have a path!
    fragment.uri.foreach { uri =>
      val diagnostics = feedback.map(toDiagnostic).asJava
      client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics))
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
