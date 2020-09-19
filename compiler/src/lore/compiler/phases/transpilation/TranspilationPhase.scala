package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.{Compilation, Phase}
import lore.compiler.semantics.Registry
import lore.compiler.types.DeclaredType
import lore.compiler.utils.CollectionExtensions.FilterTypeVectorExtension

class TranspilationPhase()(implicit compilerOptions: CompilerOptions, registry: Registry) extends Phase[String] {
  override def result: Compilation[String] = {
    val preamble = "import Lore from './runtime/src/lore/runtime/Lore.ts';"
    for {
      namedTypes <- registry.getTypesInOrder.filterType[DeclaredType].map(DeclaredTypeTranspiler.transpile).simultaneous.map(_.mkString("\n"))
      functions <- {
        registry
          .getMultiFunctions.values.map(new MultiFunctionTranspiler(_).transpile()).toVector.simultaneous
          .map(_.mkString(s"\n/* ${"=".repeat(74)} */\n\n"))
      }
    } yield List(preamble, namedTypes, functions).mkString("\n\n")
  }
}
