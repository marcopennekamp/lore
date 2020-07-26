package lore.compiler.phases.verification

import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.transformer.StmtTransformer
import lore.compiler.core.Compilation
import lore.compiler.semantics.Registry

object FunctionTransformations {
  /**
    * Applies the transformations generally necessary for function bodies before we go into the transpilation phase.
    */
  def transformBody(body: ExprNode)(implicit registry: Registry): Compilation[ExprNode] = {
    StmtTransformer.transform(new ComparisonTransformer)(body).asInstanceOf[Compilation[ExprNode]]
  }
}
