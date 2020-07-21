package lore.compiler.phases.resolution

import lore.compiler.ast.DeclNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.Registry
import lore.compiler.phases.Phase

class ResolutionPhase(declarations: List[DeclNode]) extends Phase[Registry] {
  override lazy val result: C[Registry] = (new DeclarationResolver).buildRegistry(declarations)
}
