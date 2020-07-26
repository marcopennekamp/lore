package lore.compiler.phases.resolution

import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.transformer.StmtTransformer
import lore.compiler.core.Compilation

object FunctionTransformations {
  /**
    * Applies the transformations generally necessary for function bodies before we go into the verification phase.
    */
  def transformBody(body: ExprNode): Compilation[ExprNode] = {
    StmtTransformer.transform(new ConcatToStringTransformer)(body).asInstanceOf[Compilation[ExprNode]]
  }
}
