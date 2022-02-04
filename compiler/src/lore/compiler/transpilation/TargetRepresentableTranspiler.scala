package lore.compiler.transpilation

import lore.compiler.core.CompilationException
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.semantics.scopes.{LocalVariable, StructObjectBinding}
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.target.Target.TargetExpression
import lore.compiler.target.TargetDsl.ExpressionExtension
import lore.compiler.target.TargetRepresentable
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory

object TargetRepresentableTranspiler {

  /**
    * Transpiles a given target representable entity to its target expression.
    */
  def transpile(targetRepresentable: TargetRepresentable)(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    def supportLazy(expression: TargetExpression, isLazy: Boolean): TargetExpression = {
      if (isLazy) {
        // Call the `value()` method of the LazyValue to get the actual instance.
        expression.prop("value").call()
      } else expression
    }

    targetRepresentable match {
      case module: GlobalModule => throw CompilationException(s"Modules don't have a representation in the target language. Module name: ${module.name}.")
      case variable: GlobalVariableDefinition => supportLazy(RuntimeNames.globalVariable(variable), !variable.value.isLocalized)
      case mf: MultiFunctionDefinition => RuntimeNames.multiFunction(mf)
      case function: FunctionDefinition => RuntimeNames.functionDefinition(function)
      case LocalVariable(_, name, _, _) => RuntimeNames.localVariable(name)

      case structObject: StructObjectBinding =>
        val definition = structObject.tpe.schema.definition
        val varObject = RuntimeNames.struct.`object`(structObject.tpe.schema)
        supportLazy(varObject, !definition.allDefaultsLocalized)
    }
  }

}
