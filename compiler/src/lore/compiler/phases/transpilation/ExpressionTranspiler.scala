package lore.compiler.phases.transpilation

import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.TranspiledTypeVariables
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}

object ExpressionTranspiler {

  def transpile(expression: Expression)(implicit registry: Registry, runtimeTypeVariables: TranspiledTypeVariables): Chunk = {
    ExpressionVisitor.visit(new ExpressionTranspilationVisitor())(expression)
  }

}
