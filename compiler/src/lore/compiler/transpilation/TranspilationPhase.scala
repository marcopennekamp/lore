package lore.compiler.transpilation

import lore.compiler.core.{CompilationException, CompilerOptions}
import lore.compiler.transpilation.functions.MultiFunctionTranspiler
import lore.compiler.transpilation.structures.DeclaredSchemaTranspiler
import lore.compiler.transpilation.values.{SymbolHistory, SymbolTranspiler}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.types.DeclaredSchema

object TranspilationPhase {
  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Vector[TargetStatement] = {
    implicit val symbolHistory: SymbolHistory = new SymbolHistory

    val schemaDeclarations = registry.schemasInOrder.flatMap {
      case (_, schema: DeclaredSchema) => DeclaredSchemaTranspiler.transpile(schema) :+ Target.Divider
      case _ => Vector.empty
    }

    // Transpile any additional declarations of declared schemas that require all schemas to be initialized, regardless
    // of schema order.
    val schemaDeclarationDeferredDeclarations = registry.schemasInOrder.flatMap {
      case (_, schema: DeclaredSchema) =>
        val result = DeclaredSchemaTranspiler.transpileDeferred(schema)
        if (result.nonEmpty) result :+ Target.Divider else result
      case _ => Vector.empty
    }

    val introspectionInitialization = registry.typeScope.getTraitSchema(Introspection.typeName) match {
      case None => throw CompilationException(s"The compiler should generate a trait '${Introspection.typeName}' for the introspection API.")
      case Some(introspectionType) => Vector(
        RuntimeApi.types.introspection.initialize(TypeTranspiler.transpile(introspectionType.representative)(Map.empty, symbolHistory)),
        Target.Divider,
      )
    }

    val functions = registry.multiFunctions.values.toVector.flatMap(new MultiFunctionTranspiler(_).transpile() :+ Target.Divider)

    val symbolDeclarations = SymbolTranspiler.transpile(symbolHistory) :+ Target.Divider

    symbolDeclarations ++ schemaDeclarations ++ schemaDeclarationDeferredDeclarations ++ introspectionInitialization ++ functions
  }
}
