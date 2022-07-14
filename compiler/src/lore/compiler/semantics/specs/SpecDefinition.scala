package lore.compiler.semantics.specs

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.definitions.{Definition, HasLocalModule}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.syntax.DeclNode.SpecNode
import lore.compiler.utils.Once

/**
  * A definition of a `spec`. Spec definition equality is always reference equality, as we create exactly one spec
  * definition for every defined spec. The position is restricted to the spec's description for better error reporting,
  * highlighting and indexing.
  */
class SpecDefinition(
  val description: String,
  val isTest: Boolean,
  val isBenchmark: Boolean,
  val node: SpecNode,
) extends Definition with Positioned with HasLocalModule {
  val body: Once[Expression] = new Once

  override def position: Position = node.position
}
