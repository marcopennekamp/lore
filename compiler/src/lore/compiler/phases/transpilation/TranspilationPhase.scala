package lore.compiler.phases.transpilation

import lore.compiler.core.{CompilationException, CompilerOptions}
import lore.compiler.phases.transpilation.functions.MultiFunctionTranspiler
import lore.compiler.phases.transpilation.structures.DeclaredTypeTranspiler
import lore.compiler.phases.transpilation.values.{SymbolHistory, SymbolTranspiler}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.types.DeclaredType

object TranspilationPhase {
  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Vector[TargetStatement] = {
    implicit val symbolHistory: SymbolHistory = new SymbolHistory

    val typeDeclarations = registry.schemasInOrder.flatMap {
      case (_, declaredType: DeclaredType) => DeclaredTypeTranspiler.transpile(declaredType) :+ Target.Divider
      case _ => Vector.empty
    }

    // Transpile any additional parts of declared types that require all types to be initialized, regardless of type
    // order.
    val typeDeclarationDeferredDefinitions = registry.schemasInOrder.flatMap {
      case (_, declaredType: DeclaredType) =>
        val result = DeclaredTypeTranspiler.transpileDeferred(declaredType)
        if (result.nonEmpty) result :+ Target.Divider else result
      case _ => Vector.empty
    }

    val introspectionInitialization = registry.typeScope.getTraitSchema(Introspection.typeName) match {
      case None => throw CompilationException(s"The compiler should generate a trait '${Introspection.typeName}' for the introspection API.")
      case Some(introspectionType) => Vector(
        RuntimeApi.types.introspection.initialize(TypeTranspiler.transpile(introspectionType.instantiateConstant())(Map.empty, symbolHistory)),
        Target.Divider,
      )
    }

    val functions = registry.multiFunctions.values.toVector.flatMap(new MultiFunctionTranspiler(_).transpile() :+ Target.Divider)

    val symbolDeclarations = SymbolTranspiler.transpile(symbolHistory) :+ Target.Divider

    symbolDeclarations ++ typeDeclarations ++ typeDeclarationDeferredDefinitions ++ introspectionInitialization ++ functions
  }
}
