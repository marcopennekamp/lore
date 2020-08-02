package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.C
import lore.compiler.core.Phase
import lore.compiler.semantics.Registry
import lore.compiler.syntax.DeclNode

class ResolutionPhase(declarations: List[DeclNode]) extends Phase[Registry] {
  override lazy val result: C[Registry] = (new DeclarationResolver).resolve(declarations)
}
