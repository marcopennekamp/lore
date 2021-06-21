package lore.compiler.phases.transpilation.values

import lore.compiler.phases.transpilation.{RuntimeApi, RuntimeNames}
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.VariableExtension

object SymbolTranspiler {

  /**
    * Transpiles all interned symbol types and values.
    */
  def transpile(symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    symbolHistory.symbols.toVector.flatMap(transpile)
  }

  private def transpile(name: String): Vector[TargetStatement] = {
    Vector(transpileType(name), transpileValue(name))
  }

  private def transpileType(name: String): TargetStatement = {
    RuntimeNames.symbolType(name).declareAs(RuntimeApi.symbols.tpe(name))
  }

  private def transpileValue(name: String): TargetStatement = {
    RuntimeNames.symbolValue(name).declareAs(RuntimeApi.symbols.value(name))
  }

}
