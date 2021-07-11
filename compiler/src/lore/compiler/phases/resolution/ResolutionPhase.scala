package lore.compiler.phases.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode

object ResolutionPhase {
  def process(declarations: Vector[DeclNode])(implicit reporter: Reporter): Registry = DeclarationResolver.resolve(declarations)
}
