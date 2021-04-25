package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.phases.transpilation.functions.MultiFunctionTranspiler
import lore.compiler.phases.transpilation.structures.{DeclaredTypeTranspiler, TypeAliasTranspiler}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.types.DeclaredType

object TranspilationPhase {
  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Compilation[Vector[TargetStatement]] = {
    val typeDeclarations = registry.getTypeDeclarationsInOrder.flatMap {
      case (_, declaredType: DeclaredType) => DeclaredTypeTranspiler.transpile(declaredType)
      case (name, tpe) => Vector(TypeAliasTranspiler.transpile(name, tpe))
    }.filterNot(_ == Target.Empty)

    // Transpile any additional parts of declared types that require all types to be initialized, regardless of type
    // order.
    val typeDeclarationDeferredDefinitions = registry.getTypeDeclarationsInOrder.flatMap {
      case (_, declaredType: DeclaredType) => DeclaredTypeTranspiler.transpileDeferred(declaredType)
      case _ => Vector.empty
    }

    val introspectionInitialization = registry.getTraitType(Introspection.typeName) match {
      case None => throw CompilationException(s"The compiler should generate a trait '${Introspection.typeName}' for the introspection API.")
      case Some(introspectionType) => RuntimeApi.types.introspection.initialize(TypeTranspiler.transpile(introspectionType)(Map.empty))
    }

    val functions = registry.getMultiFunctions.values.toVector.flatMap(new MultiFunctionTranspiler(_).transpile())

    (typeDeclarations ++ typeDeclarationDeferredDefinitions ++ Vector(introspectionInitialization) ++ functions).compiled
  }

  //private val divider = s"\n\n/* ${"=".repeat(74)} */\n\n"
}
