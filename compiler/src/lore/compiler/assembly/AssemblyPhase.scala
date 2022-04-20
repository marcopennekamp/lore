package lore.compiler.assembly

import com.typesafe.scalalogging.Logger
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

  val logger: Logger = Logger("lore.compiler.assembly")
  val loggerBlank: Logger = Logger("lore.compiler.assembly.blank")

  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Vector[AssembledFragment] = {
    // TODO (assembly): The assembly phase should generate PoemFragments that resemble the actual Fragments. So all
    //                  individual definitions in a fragment should be placed into the same PoemFragment. We can
    //                  achieve this by differentiating each definition's position.
    //                  Alternatively, we can build a single big `binary.poem` file, but this requires constant tables
    //                  to be unique per function. Since we probably have to do that anyway (see the corresponding VM
    //                  TODO), this is likely the best approach.
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

    logPoemFunctions(poemFunctions)

    Vector(AssembledFragment(Path.of("binary.poem"), PoemWriter.writeFragment(poemFragment)))
  }

  /**
    * Logs the generated instructions of the given poem functions. The functions are ordered by their name so that
    * individual functions (alone or in groups) can be found quickly. All Pyramid functions inside the parent module
    * `lore` will be ordered before any non-Pyramid functions.
    */
  private def logPoemFunctions(poemFunctions: Vector[PoemFunction]): Unit = {
    AssemblyPhase.logger.whenDebugEnabled {
      poemFunctions
        .sortWith { case (f1, f2) =>
          if (f1.name.hasPrefix("lore") && !f2.name.hasPrefix("lore")) true
          else if (!f1.name.hasPrefix("lore") && f2.name.hasPrefix("lore")) false
          else f1.name.toString < f2.name.toString
        }
        .foreach { function =>
          AssemblyPhase.logger.debug(s"Instructions for function `${function.signature}`:")
          function.instructions.zipWithIndex.foreach { case (instruction, index) =>
            AssemblyPhase.logger.debug(s"$index: " + instruction)
          }
          AssemblyPhase.loggerBlank.debug("")
        }
    }
  }

}
