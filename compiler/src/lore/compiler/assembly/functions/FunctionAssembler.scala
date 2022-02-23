package lore.compiler.assembly.functions

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
    val typeParameters = signature.typeParameters.map(TypeAssembler.generateParameter)
    val (instructions, additionalPoemFunctions) = body match {
      case Some(body) => generateInstructions(signature, body, capturedVariables)
      case None => (Vector.empty, Vector.empty)
    }
    val registerCount = PoemInstruction.registerCount(instructions)

    if (body.isDefined) {
      // TODO (assembly): Turn this into a trace log.
      println(s"Instructions for function ${signature.name}:")
      instructions.zipWithIndex.foreach { case (instruction, index) =>
        println(s"$index: " + instruction)
      }
      println()
    }

    PoemFunction(
      signature.name.toString,
      typeParameters,
      TypeAssembler.generate(signature.inputType),
      TypeAssembler.generate(signature.outputType),
      body.isEmpty,
      registerCount,
      instructions,
    ) +: additionalPoemFunctions
  }

  private def generateInstructions(
    signature: FunctionSignature,
    body: Expression,
    capturedVariables: CapturedVariableMap,
  )(implicit registry: Registry): (Vector[PoemInstruction], Vector[PoemFunction]) = {
    val expressionAssembler = new ExpressionAssembler(signature, capturedVariables)
    val bodyChunk = expressionAssembler.generate(body)

    var instructions = bodyChunk.instructions :+ PoemInstruction.Return(bodyChunk.forceResult(body.position))
    instructions = LabelResolver.resolve(instructions, body.position)
    instructions = ConstSmasher.optimize(instructions)
    instructions = RegisterAllocator.optimize(instructions, signature.parameters.length)

    (instructions, expressionAssembler.generatedPoemFunctions)
  }

}
