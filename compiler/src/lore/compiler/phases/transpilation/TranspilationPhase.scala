package lore.compiler.phases.transpilation

import lore.compiler.Compilation.C
import lore.compiler.Registry
import lore.compiler.phases.Phase

class TranspilationPhase()(implicit registry: Registry) extends Phase[String] {
  override def result: C[String] = {
    registry.getMultiFunctions.values.map(MultiFunctionTranspiler.transpile).toList.simultaneous.map(_.mkString("\n\n\n"))
  }
}
