package lore.lsp

import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.services._
import org.eclipse.lsp4j._

import java.util.concurrent.CompletableFuture

/**
  * Note that we have to override all methods declared in LanguageServer that have annotations, because Scala
  * incorrectly duplicates the annotations of methods that aren't overridden. Such duplicated annotations lead to
  * "duplicate RPC method" errors. See: https://github.com/eclipse/lsp4j/issues/313.
  */
class LoreLanguageServer extends LanguageServer with LanguageClientAware {

  private var client: LanguageClient = _

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    // Let's try to provide "go to definition" capabilities first.
    val capabilities = new ServerCapabilities
    capabilities.setDefinitionProvider(new DefinitionOptions)
    // TODO: Add semanticTokens support for LSP-implemented syntax highlighting.

    val serverInfo = new ServerInfo("lore-language-server")

    CompletableFutures.computeAsync { cancelToken =>

      // Initialize the given workspace as a



      /* params.getWorkspaceFolders match {
        case list if list.size() == 1 =>
          cancelToken.checkCanceled()
          new InitializeResult(capabilities, serverInfo)

        case _ =>
          cancelToken.checkCanceled()
          ??? //new InitializeError(false) // ?????
      } */

      cancelToken.checkCanceled()
      new InitializeResult(capabilities, serverInfo)
    }
  }

  override def initialized(params: InitializedParams): Unit = {
    client.showMessage(new MessageParams(MessageType.Info, "Hello, Visual Studio Code!\n- Lore language server"))
  }

  override def shutdown(): CompletableFuture[AnyRef] = CompletableFuture.completedFuture(new Object)

  override def exit(): Unit = System.exit(0)

  /* def getTextDocumentService: TextDocumentService = new TextDocumentService {
    override def didOpen(params: DidOpenTextDocumentParams): Unit = { }
    override def didChange(params: DidChangeTextDocumentParams): Unit = { }
    override def didClose(params: DidCloseTextDocumentParams): Unit = { }
    override def didSave(params: DidSaveTextDocumentParams): Unit = { }
  }

  def getWorkspaceService: WorkspaceService = new WorkspaceService {
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = { }
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = { }
  } */

  override def getTextDocumentService: TextDocumentService = null

  override def getWorkspaceService: WorkspaceService = null

  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = super.cancelProgress(params)

  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }

}
