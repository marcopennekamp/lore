package lore.lsp

import lore.compiler.build.SourceFiles
import lore.compiler.core.Fragment
import lore.compiler.feedback.Reporter
import org.eclipse.lsp4j.{TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem}

import java.nio.file.Path
import scala.collection.mutable

class FragmentManager {

  private val managedContent: mutable.Map[String, String] = mutable.HashMap()

  def get(uri: String)(implicit context: LanguageServerContext): Option[Fragment] = this.synchronized {
    implicit val reporter: Reporter = MessageLogger.freshReporter
    val path = context.workspaceFolder.relativize(Path.of(uri))

    managedContent.get(uri)
      .map(content => Fragment(uri, Some(path), content))
      .orElse(SourceFiles.ofFile(path))
  }

  def openDocument(textDocument: TextDocumentItem): Unit = this.synchronized {
    managedContent.update(textDocument.getUri, textDocument.getText)
  }

  def updateDocument(identifier: TextDocumentIdentifier, changes: Vector[TextDocumentContentChangeEvent]): Unit = this.synchronized {
    var content = managedContent.getOrElse(identifier.getUri, "")
    changes.foreach { change =>
      val range = change.getRange
      if (range != null) {
        throw new RuntimeException("Incremental text document synchronization is not supported.")
      } else {
        content = change.getText
      }
    }
    managedContent.update(identifier.getUri, content)
  }

  def closeDocument(identifier: TextDocumentIdentifier): Unit = this.synchronized {
    managedContent.remove(identifier.getUri)
  }

}
