package lore.lsp

import lore.compiler.utils.Timer.timed
import lore.lsp.capabilities.{DefinitionHandler, FeedbackPublisher, SemanticTokensHandler}
import lore.lsp.index.IndexBuilder
import lore.lsp.utils.{MessageLogger, MessageToaster}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services._

import java.nio.file.Path
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._

/**
  * Note that we have to override all methods declared in LanguageServer that have annotations, because Scala
  * incorrectly duplicates the annotations of methods that aren't overridden. Such duplicated annotations lead to
  * "duplicate RPC method" errors. See: https://github.com/eclipse/lsp4j/issues/313.
  */
class LoreLanguageServer extends LanguageServer with LanguageClientAware {

  private implicit val context: LanguageServerContext = new LanguageServerContext

  private val feedbackPublisher: FeedbackPublisher = new FeedbackPublisher

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setDefinitionProvider(new DefinitionOptions)
    capabilities.setSemanticTokensProvider(
      new SemanticTokensWithRegistrationOptions(SemanticTokensHandler.legend, new SemanticTokensServerFull(false), false)
    )

    val serverInfo = new ServerInfo("lore-language-server")

    CompletableFutures.computeAsync { cancelToken =>
      params.getWorkspaceFolders match {
        case list if list.size() == 1 =>
          cancelToken.checkCanceled()
          context.workspaceFolder = Path.of(list.get(0).getUri)
          new InitializeResult(capabilities, serverInfo)

        case _ =>
          cancelToken.checkCanceled()
          MessageLogger.info("Please open a SINGLE workspace!")
          ??? //new InitializeError(false) // ?????
      }
    }
  }

  override def initialized(params: InitializedParams): Unit = {
    applyWorkspaceChanges()
  }

  override def shutdown(): CompletableFuture[AnyRef] = CompletableFuture.completedFuture(new Object)

  override def exit(): Unit = System.exit(0)

  override def getTextDocumentService: TextDocumentService = new TextDocumentService {
    override def didOpen(params: DidOpenTextDocumentParams): Unit = context.fragmentManager.openDocument(params.getTextDocument)
    override def didChange(params: DidChangeTextDocumentParams): Unit = context.fragmentManager.updateDocument(params.getTextDocument, params.getContentChanges.asScala.toVector)
    override def didClose(params: DidCloseTextDocumentParams): Unit = context.fragmentManager.closeDocument(params.getTextDocument)
    override def didSave(params: DidSaveTextDocumentParams): Unit = ()

    override def definition(params: DefinitionParams) = {
      CompletableFutures.computeAsync { cancelToken =>
        val result = DefinitionHandler.definition(params.getTextDocument.getUri, params.getPosition)
        cancelToken.checkCanceled()
        result match {
          case Some(locations) => Either.forLeft(locations.asJava)
          case None => null
        }
      }
    }

    override def semanticTokensFull(params: SemanticTokensParams) = {
      CompletableFutures.computeAsync { cancelToken =>
        val result = SemanticTokensHandler.semanticTokens(params.getTextDocument.getUri)
        cancelToken.checkCanceled()
        result match {
          case Some(result) => new SemanticTokens(result.map(Integer.valueOf).asJava)
          case None => throw new RuntimeException("Semantic tokens cannot be created: The file does not exist or cannot be parsed.")
        }
      }
    }

    // Override all methods to avoid Scala-specific annotation errors.
    override def completion(position: CompletionParams) = super.completion(position)
    override def resolveCompletionItem(unresolved: CompletionItem) = super.resolveCompletionItem(unresolved)
    override def hover(params: HoverParams) = super.hover(params)
    override def signatureHelp(params: SignatureHelpParams) = super.signatureHelp(params)
    override def declaration(params: DeclarationParams) = super.declaration(params)
    override def typeDefinition(params: TypeDefinitionParams) = super.typeDefinition(params)
    override def implementation(params: ImplementationParams) = super.implementation(params)
    override def references(params: ReferenceParams) = super.references(params)
    override def documentHighlight(params: DocumentHighlightParams) = super.documentHighlight(params)
    override def documentSymbol(params: DocumentSymbolParams) = super.documentSymbol(params)
    override def codeAction(params: CodeActionParams) = super.codeAction(params)
    override def resolveCodeAction(unresolved: CodeAction) = super.resolveCodeAction(unresolved)
    override def codeLens(params: CodeLensParams) = super.codeLens(params)
    override def resolveCodeLens(unresolved: CodeLens) = super.resolveCodeLens(unresolved)
    override def formatting(params: DocumentFormattingParams) = super.formatting(params)
    override def rangeFormatting(params: DocumentRangeFormattingParams) = super.rangeFormatting(params)
    override def onTypeFormatting(params: DocumentOnTypeFormattingParams) = super.onTypeFormatting(params)
    override def rename(params: RenameParams) = super.rename(params)
    override def linkedEditingRange(params: LinkedEditingRangeParams) = super.linkedEditingRange(params)
    override def willSave(params: WillSaveTextDocumentParams): Unit = super.willSave(params)
    override def willSaveWaitUntil(params: WillSaveTextDocumentParams) = super.willSaveWaitUntil(params)
    override def documentLink(params: DocumentLinkParams) = super.documentLink(params)
    override def documentLinkResolve(params: DocumentLink) = super.documentLinkResolve(params)
    override def documentColor(params: DocumentColorParams) = super.documentColor(params)
    override def colorPresentation(params: ColorPresentationParams) = super.colorPresentation(params)
    override def foldingRange(params: FoldingRangeRequestParams) = super.foldingRange(params)
    override def prepareRename(params: PrepareRenameParams) = super.prepareRename(params)
    override def typeHierarchy(params: TypeHierarchyParams) = super.typeHierarchy(params)
    override def resolveTypeHierarchy(params: ResolveTypeHierarchyItemParams) = super.resolveTypeHierarchy(params)
    override def prepareCallHierarchy(params: CallHierarchyPrepareParams) = super.prepareCallHierarchy(params)
    override def callHierarchyIncomingCalls(params: CallHierarchyIncomingCallsParams) = super.callHierarchyIncomingCalls(params)
    override def callHierarchyOutgoingCalls(params: CallHierarchyOutgoingCallsParams) = super.callHierarchyOutgoingCalls(params)
    override def selectionRange(params: SelectionRangeParams) = super.selectionRange(params)
    override def semanticTokensFullDelta(params: SemanticTokensDeltaParams) = super.semanticTokensFullDelta(params)
    override def semanticTokensRange(params: SemanticTokensRangeParams) = super.semanticTokensRange(params)
    override def moniker(params: MonikerParams) = super.moniker(params)
  }

  override def getWorkspaceService: WorkspaceService = new WorkspaceService {
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ???
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = applyWorkspaceChanges()
  }

  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = super.cancelProgress(params)

  override def connect(client: LanguageClient): Unit = {
    context.client = client
  }

  private def applyWorkspaceChanges(): Unit = this.synchronized {
    val (registry, reporter) = WorkspaceAnalyzer.analyze()
    context.registry = registry
    context.globalIndex = timed("Building the index", log = MessageLogger.info)(IndexBuilder.fromRegistry(registry))

    if (reporter.hasErrors) {
      MessageToaster.info("Workspace compilation failed.")
    }

    feedbackPublisher.publish(reporter.feedback)
  }

}
