package lore.compiler.transpilation

import lore.compiler.core.CompilerOptions
import lore.compiler.semantics.Registry
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.transpilation.functions.MultiFunctionTranspiler
import lore.compiler.transpilation.structures.DeclaredSchemaTranspiler
import lore.compiler.transpilation.values.{SymbolHistory, SymbolTranspiler}
import lore.compiler.transpilation.variables.GlobalVariableTranspiler
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

    val introspectionInitialization = {
      val tpe = TypeTranspiler.transpile(registry.core.Type.schema.get.representative)(Map.empty, symbolHistory)
      Vector(
        RuntimeApi.types.introspection.initialize(tpe),
        Target.Divider,
      )
    }

    val globalVariables = registry.bindings.globalVariables.values.toVector.flatMap(GlobalVariableTranspiler.transpile(_) :+ Target.Divider)
    val functions = registry.bindings.multiFunctions.values.toVector.flatMap(new MultiFunctionTranspiler(_).transpile() :+ Target.Divider)

    // We have to transpile symbol declarations last, because we first need to transpile everything else to fill the
    // symbol history.
    val symbolDeclarations = SymbolTranspiler.transpile(symbolHistory) :+ Target.Divider

    symbolDeclarations ++ schemaDeclarations ++ schemaDeclarationDeferredDeclarations ++ introspectionInitialization ++ globalVariables ++ functions
  }
}
