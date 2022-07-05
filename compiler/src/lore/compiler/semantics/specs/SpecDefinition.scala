package lore.compiler.semantics.specs

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.definitions.{Definition, HasLocalModule}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.syntax.DeclNode.SpecNode

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

  /**
    * This is a variable because it will be transformed during the course of the compilation.
    */
  var body: Expression = _

  override def position: Position = node.position

}
