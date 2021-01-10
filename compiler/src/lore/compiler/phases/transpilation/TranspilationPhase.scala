package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.{Compilation, Phase}
import lore.compiler.semantics.Registry
import lore.compiler.types.DeclaredType

class TranspilationPhase()(implicit compilerOptions: CompilerOptions, registry: Registry) extends Phase[String] {
  override def result: Compilation[String] = {
    val preamble = "import Lore from './runtime/src/lore/runtime/Lore.ts';"
    for {
      typeDeclarations <- registry.getTypeDeclarationsInOrder.map {
        case (_, declaredType: DeclaredType) => DeclaredTypeTranspiler.transpile(declaredType)
        case (name, tpe) => TypeAliasTranspiler.transpile(name, tpe)
      }.simultaneous.map(_.mkString("\n"))
      functions <- {
        registry
          .getMultiFunctions.values.map(new MultiFunctionTranspiler(_).transpile()).toVector.simultaneous
          .map(_.mkString(s"\n/* ${"=".repeat(74)} */\n\n"))
      }
    } yield List(preamble, typeDeclarations, functions).mkString("\n\n")
  }
}
