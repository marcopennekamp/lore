package lore.compiler.assembly

import lore.compiler.assembly.expressions.ExpressionAssembler
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemFunction, PoemInstruction}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition

object FunctionAssembler {

  def generate(function: FunctionDefinition)(implicit registry: Registry): PoemFunction = {
    val typeParameters = function.typeParameters.map(TypeAssembler.generateParameter)
    val instructions = function.body.map(
      body => ExpressionAssembler.generate(function.signature.parameters, body)
    ).getOrElse(Vector.empty)
    val registerCount = PoemInstruction.registerCount(instructions)

    if (!function.isAbstract) {
      println(s"Instructions for function ${function.name}:")
      instructions.zipWithIndex.foreach { case (instruction, index) =>
        println(s"$index: " + instruction)
      }
      println()
    }

    PoemFunction(
      function.name.toString,
      typeParameters,
      TypeAssembler.generate(function.signature.inputType),
      TypeAssembler.generate(function.signature.outputType),
      function.isAbstract,
      registerCount,
      instructions,
    )
  }

}
