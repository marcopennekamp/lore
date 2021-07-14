package lore.lsp

import lore.compiler.semantics.Registry
import lore.lsp.index.GlobalIndex
import org.eclipse.lsp4j.services.LanguageClient

import java.nio.file.Path

class LanguageServerContext {
  var client: LanguageClient = _
  var workspaceFolder: Path = _
  var registry: Registry = _
  var globalIndex: GlobalIndex = _
  val fragmentManager: FragmentManager = new FragmentManager
}
