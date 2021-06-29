package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, CompilerOptions}
import lore.compiler.phases.transpilation.functions.MultiFunctionTranspiler
import lore.compiler.phases.transpilation.structures.{DeclaredTypeTranspiler, TypeAliasTranspiler}
import lore.compiler.phases.transpilation.values.{SymbolHistory, SymbolTranspiler}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.types.DeclaredType

object TranspilationPhase {
  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Compilation[Vector[TargetStatement]] = {
    implicit val symbolHistory: SymbolHistory = new SymbolHistory

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
      case Some(introspectionType) => RuntimeApi.types.introspection.initialize(TypeTranspiler.transpile(introspectionType)(Map.empty, symbolHistory))
    }

    val functions = registry.getMultiFunctions.values.toVector.flatMap(new MultiFunctionTranspiler(_).transpile())

    val symbolDeclarations = SymbolTranspiler.transpile(symbolHistory)

    (symbolDeclarations ++ typeDeclarations ++ typeDeclarationDeferredDefinitions ++ Vector(introspectionInitialization) ++ functions).compiled
  }

  // TODO: Add dividers as special target nodes.
  //private val divider = s"\n\n/* ${"=".repeat(74)} */\n\n"
}
