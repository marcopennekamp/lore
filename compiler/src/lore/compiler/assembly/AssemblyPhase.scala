package lore.compiler.assembly

import lore.compiler.core.CompilerOptions
import lore.compiler.poem.{PoemFragment, PoemWriter}
import lore.compiler.semantics.Registry

import java.nio.file.Path

object AssemblyPhase {

  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Vector[AssembledFragment] = {
    // TODO (assembly): The assembly phase should generate PoemFragments that resemble the actual Fragments. So all
    //                  individual definitions in a fragment should be placed into the same PoemFragment. We can
    //                  achieve this by differentiating each definition's position.
    /* val schemaDeclarations = registry.schemasInOrder.flatMap {
      case (_, schema: DeclaredSchema) => DeclaredSchemaTranspiler.transpile(schema) :+ Target.Divider
      case _ => Vector.empty
    } */
    // val globalVariables = registry.bindings.globalVariables.values.toVector.flatMap(GlobalVariableTranspiler.transpile(_) :+ Target.Divider)
    val poemFunctions = registry.bindings.multiFunctions.values.toVector.flatMap(_.functions).map(FunctionAssembler.generate)
    val poemFragment = PoemFragment(poemFunctions)

    // TODO (assembly): Support introspection (probably needs VM support).
    /* val introspectionInitialization = {
      val tpe = TypeTranspiler.transpile(registry.core.Type.schema.get.representative)(Map.empty, symbolHistory)
      Vector(
        RuntimeApi.types.introspection.initialize(tpe),
        Target.Divider,
      )
    } */

    Vector(AssembledFragment(Path.of("lore_program.poem"), PoemWriter.write(poemFragment)))
  }

}
