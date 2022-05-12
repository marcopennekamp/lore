package lore.compiler.semantics.specs

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{Definition, NamePath}
import lore.compiler.syntax.ExprNode

/**
  * A definition of a `spec`. Spec definition equality is always reference equality, as we create exactly one spec
  * definition for every defined spec. The position is restricted to the spec's name for better test and benchmark
  * reporting, as well as error highlighting and indexing.
  */
class SpecDefinition(
  override val name: NamePath,
  val isTest: Boolean,
  val isBenchmark: Boolean,
  val bodyNode: ExprNode,
  val localModule: LocalModule,
  override val position: Position,
) extends Definition with Positioned {

  /**
    * This is a variable because it will be transformed during the course of the compilation.
    */
  var body: Expression = _

}
