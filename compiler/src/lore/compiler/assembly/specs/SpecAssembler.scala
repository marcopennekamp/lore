package lore.compiler.assembly.specs

import lore.compiler.assembly.RuntimeNames
import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.poem.{PoemFunction, PoemSpec}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.types.TupleType

object SpecAssembler {

  /**
    * Generates a PoemSpec from `spec` and its execution function, including any auxiliary functions.
    */
  def generate(spec: SpecDefinition)(implicit registry: Registry): (PoemSpec, Vector[PoemFunction]) = {
    val executableName = RuntimeNames.spec.executable(spec)
    val signature = FunctionSignature.constant(executableName, TupleType.UnitType, spec.position)
    val poemFunctions = FunctionAssembler.generate(signature, Some(spec.body), Map.empty)
    val poemSpec = PoemSpec(spec.name, spec.isTest, spec.isBenchmark, executableName)
    (poemSpec, poemFunctions)
  }

}
