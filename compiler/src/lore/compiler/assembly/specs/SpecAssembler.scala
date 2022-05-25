package lore.compiler.assembly.specs

import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.poem.{PoemFunction, PoemSpec}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.types.TupleType

import java.util.UUID

object SpecAssembler {

  /**
    * Generates a PoemSpec from `spec` and its execution function, including any auxiliary functions.
    */
  def generate(spec: SpecDefinition)(implicit registry: Registry): (PoemSpec, Vector[PoemFunction]) = {
    // There is no need to have stable executable names across compilation runs, so a UUID suffices.
    val executableName = spec.localModule.modulePath.appendToLastSegment("/spec-" + UUID.randomUUID())
    val signature = FunctionSignature.constant(executableName, TupleType.UnitType, spec.position)
    val poemFunctions = FunctionAssembler.generate(signature, Some(spec.body), Map.empty)
    val poemSpec = PoemSpec(spec.localModule.modulePath, spec.description, spec.isTest, spec.isBenchmark, executableName)
    (poemSpec, poemFunctions)
  }

}