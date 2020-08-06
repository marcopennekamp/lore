package lore.compiler.phases.resolution

import lore.compiler.core.{Compilation, Phase}
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode

class ResolutionPhase(declarations: List[DeclNode]) extends Phase[Registry] {
  override lazy val result: Compilation[Registry] = (new DeclarationResolver).resolve(declarations)
}
