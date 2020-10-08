package lore.compiler.phases.transpilation

import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.Transpilation.Transpilation
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}

object ExpressionTranspiler {

  def transpile(expression: Expression)(implicit registry: Registry, runtimeTypeVariables: RuntimeTypeVariables): Transpilation = {
    ExpressionVisitor.visit(new ExpressionTranspilationVisitor())(expression)
  }

}
