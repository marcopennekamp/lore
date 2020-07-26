package lore.compiler.semantics.functions

import lore.compiler.core.Compilation.C
import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.verification.LocalVariable
import lore.compiler.types.{Type, TypingDeferred}

// TODO: Refactor: Create a trait ParameterDefinition and two classes: DeferredParameterDefinition and
//       ResolvedParameterDefinition. Then during type resolution, manually convert Deferred to Resolved
//       objects. This has the advantage of hiding the whole resolution stuff behind a curtain.
//       To get even more type-safe, we could of course consider introducing pre- and post-resolution data
//       structures, but this seems to be a little overkill.

class ParameterDefinition(
  val name: String,
  override val typeResolver: () => C[Type],
  override val position: Position,
) extends Positioned with TypingDeferred[Type] {
  override def toString = s"$name: $tpe"
  def asLocalVariable: LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
