package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.{Compilation, Phase}
import lore.compiler.semantics.Registry

class TranspilationPhase()(implicit compilerOptions: CompilerOptions, registry: Registry) extends Phase[String] {
  override def result: Compilation[String] = {
    val preamble = "import Lore from './runtime/src/lore/runtime/Lore.ts';"
    for {
      namedTypes <- registry.getTypesInOrder.map(NamedTypeTranspiler.transpile).toList.simultaneous.map(_.mkString("\n"))
      functions <- {
        registry
          .getMultiFunctions.values.map(new MultiFunctionTranspiler(_).transpile()).toList.simultaneous
          .map(_.mkString(s"\n/* ${"=".repeat(74)} */\n\n"))
      }
    } yield List(preamble, namedTypes, functions).mkString("\n\n")
  }
}
