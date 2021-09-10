package lore.compiler.transpilation.variables

import lore.compiler.semantics.Registry
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.{ExpressionExtension, VariableExtension}
import lore.compiler.transpilation.{RuntimeApi, RuntimeNames}
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.expressions.ExpressionTranspiler
import lore.compiler.transpilation.values.SymbolHistory

object GlobalVariableTranspiler {

  def transpile(variable: GlobalVariableDefinition)(implicit registry: Registry, symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    val varVariable = RuntimeNames.globalVariable(variable)
    val varGetValue = RuntimeNames.globalVariable.getValue(variable)

    implicit val runtimeTypeVariables: RuntimeTypeVariables = Map.empty
    val valueChunk = ExpressionTranspiler.transpile(variable.value)

    // We're transpiling a "get_value" function so that the local variables declared in the value expression are
    // properly scoped. This function is then called during initialization of the global variable.
    val getValueDeclaration = Target.Function(varGetValue.name, Vector.empty, Target.Block(
      valueChunk.statements :+ Target.Return(valueChunk.expression)
    ))

    val variableDeclaration = if (variable.value.isLocalized) {
      varVariable.exportAs(varGetValue.call())
    } else {
      varVariable.exportAs(RuntimeApi.utils.`lazy`.of(varGetValue.call()))
    }

    Vector(getValueDeclaration, variableDeclaration)
  }

}
