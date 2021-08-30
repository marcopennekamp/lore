package lore.compiler.transpilation.values

import lore.compiler.transpilation.{RuntimeApi, RuntimeNames}
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.VariableExtension

object SymbolTranspiler {

  /**
    * Transpiles all interned symbol types and values.
    */
  def transpile(symbolHistory: SymbolHistory): Vector[TargetStatement] = symbolHistory.symbols.toVector.flatMap(transpile)

  private def transpile(name: String): Vector[TargetStatement] = {
    val targetType = RuntimeNames.symbolType(name)
    val targetValue = RuntimeNames.symbolValue(name)

    Vector(
      targetType.declareAs(RuntimeApi.symbols.tpe(name)),
      targetValue.declareAs(RuntimeApi.symbols.value(targetType))
    )
  }

}
