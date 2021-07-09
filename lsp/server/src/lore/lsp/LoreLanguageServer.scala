package lore.lsp

import lore.compiler.semantics.Registry
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.services._
import org.eclipse.lsp4j._

import java.nio.file.Path
import java.util.concurrent.CompletableFuture

/**
  * Note that we have to override all methods declared in LanguageServer that have annotations, because Scala
  * incorrectly duplicates the annotations of methods that aren't overridden. Such duplicated annotations lead to
  * "duplicate RPC method" errors. See: https://github.com/eclipse/lsp4j/issues/313.
  */
class LoreLanguageServer extends LanguageServer with LanguageClientAware {

  private implicit var client: LanguageClient = _

  private var workspaceFolder: Path = _
  private var registry: Registry = _

  private val feedbackPublisher: FeedbackPublisher = new FeedbackPublisher

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    // Let's try to provide "go to definition" capabilities first.
    val capabilities = new ServerCapabilities
    capabilities.setDefinitionProvider(new DefinitionOptions)
    // TODO: Add semanticTokens support for LSP-implemented syntax highlighting?

    val serverInfo = new ServerInfo("lore-language-server")

    CompletableFutures.computeAsync { cancelToken =>
      params.getWorkspaceFolders match {
        case list if list.size() == 1 =>
          cancelToken.checkCanceled()
          workspaceFolder = Path.of(list.get(0).getUri)
          new InitializeResult(capabilities, serverInfo)

        case _ =>
          cancelToken.checkCanceled()
          MessageLogger.info("Please open a SINGLE workspace!")
          ??? //new InitializeError(false) // ?????
      }
    }
  }

  override def initialized(params: InitializedParams): Unit = {
    client.showMessage(new MessageParams(MessageType.Info, "Lore: Initializing workspace..."))
    applyWorkspaceChanges()
  }

  override def shutdown(): CompletableFuture[AnyRef] = CompletableFuture.completedFuture(new Object)

  override def exit(): Unit = System.exit(0)

  /* def getTextDocumentService: TextDocumentService = new TextDocumentService {
    override def didOpen(params: DidOpenTextDocumentParams): Unit = { }
    override def didChange(params: DidChangeTextDocumentParams): Unit = { }
    override def didClose(params: DidCloseTextDocumentParams): Unit = { }
    override def didSave(params: DidSaveTextDocumentParams): Unit = { }
  } */

  override def getTextDocumentService: TextDocumentService = null

  override def getWorkspaceService: WorkspaceService = new WorkspaceService {
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ???
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = applyWorkspaceChanges()
  }

  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = super.cancelProgress(params)

  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }

  private def applyWorkspaceChanges(): Unit = this.synchronized {
    val (registry, reporter) = WorkspaceAnalyzer.analyze()
    val message = if (reporter.hasErrors) "Lore: Workspace compilation failed." else "Lore: Workspace compilation succeeded."

    this.registry = registry
    client.showMessage(new MessageParams(MessageType.Info, message))
    feedbackPublisher.publish(reporter.feedback)
  }

}
