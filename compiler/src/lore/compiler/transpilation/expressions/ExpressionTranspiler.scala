package lore.compiler.transpilation.expressions

import lore.compiler.transpilation.Chunk
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}

object ExpressionTranspiler {

  def transpile(expression: Expression)(implicit registry: Registry, runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): Chunk = {
    ExpressionVisitor.visit(new ExpressionTranspilationVisitor())(expression)
  }

}
