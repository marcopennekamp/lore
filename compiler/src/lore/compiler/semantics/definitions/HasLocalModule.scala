package lore.compiler.semantics.definitions

import lore.compiler.semantics.modules.{GlobalModule, LocalModule}
import lore.compiler.syntax.DeclNode

trait HasLocalModule {
  def node: DeclNode

  def localModule: LocalModule = node.localModule
  def globalModule: GlobalModule = node.localModule.globalModule
}
