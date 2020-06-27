package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation.C
import lore.compiler.core.Registry
import lore.compiler.phases.Phase

class TranspilationPhase()(implicit compilerOptions: CompilerOptions, registry: Registry) extends Phase[String] {
  override def result: C[String] = {
    registry
      .getMultiFunctions.values.map(new MultiFunctionTranspiler(_).transpile).toList.simultaneous
      .map(_.mkString(s"\n/* ${"=".repeat(74)} */\n\n"))
  }
}
