package lore.compiler.assembly.functions

import lore.compiler.assembly.AsmChunk
import lore.compiler.assembly.optimization.{ConstSmasher, RegisterAllocator}
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemFunction, PoemInstruction}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{FunctionDefinition, FunctionSignature}
import lore.compiler.types.TupleType

object FunctionAssembler {

  /**
    * Generates PoemFunctions from the given function definition. Multiple PoemFunctions are generated when the
    * function's body contains lambda expressions.
    */
  def generate(function: FunctionDefinition)(implicit registry: Registry): Vector[PoemFunction] = {
    generate(function.signature, function.body, Map.empty)
  }

  /**
    * Generates PoemFunctions from the given function signature and body. `capturedVariableMap` is only used if `body`
    * is an [[Expression]].
    */
  def generate(
    signature: FunctionSignature,
    body: Either[Expression, AsmChunk],
    capturedVariables: CapturedVariableMap = Map.empty,
  )(implicit registry: Registry): Vector[PoemFunction] = {
    body match {
      case Left(body) => generate(signature, Some(body), capturedVariables)
      case Right(bodyChunk) => Vector(generate(signature, Some(bodyChunk)))
    }
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
      case Some(bodyChunk) => finalizeBody(signature, bodyChunk)
      case None => Vector.empty
    }

    val typeParameters = signature.typeParameters.map(TypeAssembler.generateParameter)
    val registerCount = PoemInstruction.registerCount(instructions)
    PoemFunction(
      signature,
      typeParameters,
      TypeAssembler.generate(signature.inputType),
      TypeAssembler.generate(signature.outputType),
      bodyChunk.isEmpty,
      registerCount,
      instructions,
    )
  }

  private def finalizeBody(signature: FunctionSignature, bodyChunk: AsmChunk): Vector[PoemInstruction] = {
    // We have to either return the result of `bodyChunk` from the function, or return the unit value if `bodyChunk`
    // has no result. If the last instruction of the body is already a return instruction, we don't need to add any
    // additional instructions.
    val returnInstructions = bodyChunk.result match {
      case Some(regValue) => Vector(PoemInstruction.Return(regValue))
      case None if bodyChunk.instructions.lastOption.exists(PoemInstruction.isReturn) => Vector.empty
      case None =>
        if (TupleType.UnitType </= signature.outputType) {
          throw CompilationException("A block with a unit result can only be returned from a function that has a Unit" +
            s" or Any output type. Position: ${signature.position}.")
        }

        // We can always use register 0, because we're at the end of the function and we have no meaningful live
        // variables.
        val regResult = Poem.Register(0)
        Vector(
          PoemInstruction.Tuple(regResult, Vector.empty),
          PoemInstruction.Return(regResult)
        )
    }

    // Label resolution is the last step so that preceding optimization steps can add or remove instructions without
    // having to recompute absolute locations. This requires all optimization steps before label resolution to preserve
    // labels. Register allocation cannot be executed after label resolution because some optimizations rely on
    // optimized registers. This requires the RegisterAllocator to compute absolute locations for the liveness
    // information on its own, but the resulting flexibility in optimizations is well worth it.
    var instructions = bodyChunk.instructions ++ returnInstructions
    instructions = ConstSmasher.optimize(instructions)

    RegisterAllocator.logger.trace(s"Register allocation for function `$signature`:")
    instructions = RegisterAllocator.optimize(instructions, signature.parameters.length)
    RegisterAllocator.loggerBlank.trace("")

    instructions = LabelResolver.resolve(instructions)
    instructions
  }

}
