package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode

object ResolutionPhase {
  def process(declarations: Vector[DeclNode]): Compilation[Registry] = (new DeclarationResolver).resolve(declarations)
}
