package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode

object ResolutionPhase {
  def process(fragmentModules: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): Registry = DeclarationResolver.resolve(fragmentModules)
}
