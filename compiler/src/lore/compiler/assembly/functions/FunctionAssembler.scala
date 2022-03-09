package lore.compiler.assembly.functions

import lore.compiler.assembly.AsmChunk
import lore.compiler.assembly.optimization.{ConstSmasher, RegisterAllocator}
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemFunction, PoemInstruction}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature}

object FunctionAssembler {

  /**
    * Generates PoemFunctions from the given function definition. Multiple PoemFunctions are generated when the
    * function's body contains lambda expressions.
    */
  def generate(function: FunctionDefinition)(implicit registry: Registry): Vector[PoemFunction] = {
    generate(function.signature, function.body, Map.empty)
  }

  /**
    * Generates PoemFunctions from the given function signature and body. `capturedVariables` will contain entries if
    * the current function to be compiled is a lambda function with captured variables.
    */
  def generate(
    signature: FunctionSignature,
    body: Option[Expression],
    capturedVariables: CapturedVariableMap,
  )(implicit registry: Registry): Vector[PoemFunction] = {
    val (bodyChunk, additionalPoemFunctions) = body match {
      case Some(body) =>
        val expressionAssembler = new ExpressionAssembler(signature, capturedVariables)
        val bodyChunk = expressionAssembler.generate(body)
        (Some(bodyChunk), expressionAssembler.generatedPoemFunctions)

      case None => (None, Vector.empty)
    }

    generate(signature, bodyChunk) +: additionalPoemFunctions
  }

  /**
    * Generate a PoemFunction from the given function signature and body chunk. This can be used in cases where a body
    * [[Expression]] is not available, such as with constructor functions.
    */
  def generate(signature: FunctionSignature, bodyChunk: Option[AsmChunk])(implicit registry: Registry): PoemFunction = {
    val instructions = bodyChunk match {
      case Some(bodyChunk) =>
        val instructions = finalizeBody(signature, bodyChunk)

        // TODO (assembly): Turn this into a trace log.
        println(s"Instructions for function ${signature.name}:")
        instructions.zipWithIndex.foreach { case (instruction, index) =>
          println(s"$index: " + instruction)
        }
        println()

        instructions

      case None => Vector.empty
    }

    val typeParameters = signature.typeParameters.map(TypeAssembler.generateParameter)
    val registerCount = PoemInstruction.registerCount(instructions)
    PoemFunction(
      signature.name,
      typeParameters,
      TypeAssembler.generate(signature.inputType),
      TypeAssembler.generate(signature.outputType),
      bodyChunk.isEmpty,
      registerCount,
      instructions,
    )
  }

  private def finalizeBody(signature: FunctionSignature, bodyChunk: AsmChunk): Vector[PoemInstruction] = {
    var instructions = bodyChunk.instructions :+ PoemInstruction.Return(bodyChunk.forceResult(signature.position))
    instructions = LabelResolver.resolve(instructions, signature.position)
    instructions = ConstSmasher.optimize(instructions)
    instructions = RegisterAllocator.optimize(instructions, signature.parameters.length)
    instructions
  }

}
