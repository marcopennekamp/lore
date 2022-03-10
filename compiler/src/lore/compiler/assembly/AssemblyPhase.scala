package lore.compiler.assembly

import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.assembly.globals.GlobalVariableAssembler
import lore.compiler.assembly.schemas.DeclaredSchemaAssembler
import lore.compiler.core.CompilerOptions
import lore.compiler.poem.{PoemFragment, PoemFunction, PoemGlobalVariable, PoemSchema}
import lore.compiler.poem.writer.PoemWriter
import lore.compiler.semantics.Registry
import lore.compiler.types.DeclaredSchema

import java.nio.file.Path

object AssemblyPhase {

  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Vector[AssembledFragment] = {
    // TODO (assembly): The assembly phase should generate PoemFragments that resemble the actual Fragments. So all
    //                  individual definitions in a fragment should be placed into the same PoemFragment. We can
    //                  achieve this by differentiating each definition's position.
    var poemSchemas = Vector.empty[PoemSchema]
    var poemGlobalVariables = Vector.empty[PoemGlobalVariable]
    var poemFunctions = Vector.empty[PoemFunction]

    registry.schemasInOrder.foreach {
      case (_ , schema: DeclaredSchema) =>
        val (poemSchema, poemSchemaFunctions, poemSchemaObject) = DeclaredSchemaAssembler.generate(schema)
        poemSchemas :+= poemSchema
        poemSchemaObject.foreach(poemGlobalVariables :+= _)
        poemFunctions ++= poemSchemaFunctions

      case _ =>
    }

    registry.bindings.globalVariables.values.toVector.foreach { global =>
      val (poemGlobalVariable, poemInitializerFunctions) = GlobalVariableAssembler.generate(global)
      poemGlobalVariables :+= poemGlobalVariable
      poemFunctions ++= poemInitializerFunctions
    }

    registry.bindings.multiFunctions.values.toVector.flatMap(_.functions).foreach { function =>
      poemFunctions ++= FunctionAssembler.generate(function)
    }

    val poemFragment = PoemFragment(poemSchemas, poemGlobalVariables, poemFunctions)

    // TODO (assembly): Support introspection (probably needs VM support).
    /* val introspectionInitialization = {
      val tpe = TypeTranspiler.transpile(registry.core.Type.schema.get.representative)(Map.empty, symbolHistory)
      Vector(
        RuntimeApi.types.introspection.initialize(tpe),
        Target.Divider,
      )
    } */

    Vector(AssembledFragment(Path.of("binary.poem"), PoemWriter.writeFragment(poemFragment)))
  }

}
