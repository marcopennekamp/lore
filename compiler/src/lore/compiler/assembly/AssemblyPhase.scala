package lore.compiler.assembly

import com.typesafe.scalalogging.Logger
import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.assembly.globals.GlobalVariableAssembler
import lore.compiler.assembly.schemas.DeclaredSchemaAssembler
import lore.compiler.assembly.specs.SpecAssembler
import lore.compiler.core.CompilerOptions
import lore.compiler.poem._
import lore.compiler.poem.writer.PoemWriter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition

object AssemblyPhase {

  val logger: Logger = Logger("lore.compiler.assembly")
  val loggerBlank: Logger = Logger("lore.compiler.assembly.blank")

  /**
    * The assembly phase generates the bytecode of a single Poem, as the whole program will be placed into a single
    * file.
    */
  def process(implicit compilerOptions: CompilerOptions, registry: Registry): Array[Byte] = {
    var poemSchemas = Vector.empty[PoemSchema]
    var poemGlobalVariables = Vector.empty[PoemGlobalVariable]
    var poemFunctions = Vector.empty[PoemFunction]
    var poemSpecs = Vector.empty[PoemSpec]

    registry.modules.foreach { globalModule =>
      globalModule.declaredSchemas.foreach { schema =>
        val (poemSchema, poemSchemaFunctions, poemSchemaObject) = DeclaredSchemaAssembler.generate(schema)
        poemSchemas :+= poemSchema
        poemSchemaObject.foreach(poemGlobalVariables :+= _)
        poemFunctions ++= poemSchemaFunctions
      }

      globalModule.terms.all.foreach {
        case globalVariable: GlobalVariableDefinition =>
          val (poemGlobalVariable, poemInitializerFunctions) = GlobalVariableAssembler.generate(globalVariable)
          poemGlobalVariables :+= poemGlobalVariable
          poemFunctions ++= poemInitializerFunctions

        case mf: MultiFunctionDefinition =>
          mf.functions.foreach { function =>
            poemFunctions ++= FunctionAssembler.generate(function)
          }

        case _ =>
      }

      globalModule.specs.foreach { spec =>
        val (poemSpec, poemSpecFunction) = SpecAssembler.generate(spec)
        poemSpecs :+= poemSpec
        poemFunctions ++= poemSpecFunction
      }
    }

    val poemFragment = PoemFragment(poemSchemas, poemGlobalVariables, poemFunctions, poemSpecs)
    logPoemFunctions(poemFunctions)
    PoemWriter.writeFragment(poemFragment)
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
