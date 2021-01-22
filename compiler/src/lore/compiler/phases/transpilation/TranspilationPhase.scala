package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Phase}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.types.DeclaredType

class TranspilationPhase()(implicit compilerOptions: CompilerOptions, registry: Registry) extends Phase[String] {
  override def result: Compilation[String] = {
    val preamble = "import Lore from './runtime/src/lore/runtime/Lore.ts';"
    for {
      typeDeclarations <- registry.getTypeDeclarationsInOrder.map {
        case (_, declaredType: DeclaredType) => DeclaredTypeTranspiler.transpile(declaredType)
        case (name, tpe) => TypeAliasTranspiler.transpile(name, tpe)
      }.simultaneous.map(_.filterNot(_.isBlank).mkString(divider))
      introspectionInitialization <- registry.getTraitType(Introspection.typeName) match {
        case None => throw CompilationException("The compiler should generate a trait 'Type' for the introspection API.")
        case Some(introspectionType) =>
          s"${RuntimeApi.types.introspection.initialize}(${RuntimeTypeTranspiler.transpile(introspectionType)(Map.empty)})".compiled
      }
      functions <- {
        registry
          .getMultiFunctions.values.map(new MultiFunctionTranspiler(_).transpile()).toVector.simultaneous
          .map(_.mkString(divider))
      }
    } yield List(preamble, typeDeclarations, introspectionInitialization, functions).mkString(divider)
  }

  private val divider = s"\n\n/* ${"=".repeat(74)} */\n\n"
}
