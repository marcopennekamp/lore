package lore.compiler.assembly.functions

import lore.compiler.assembly.{Chunk, RegisterProvider}
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemFunction, PoemInstruction, PoemSingleFunctionValue}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.LambdaValue
import lore.compiler.semantics.functions.{FunctionSignature, ParameterDefinition}

import java.util.UUID

object LambdaAssembler {

  def generate(
    expression: LambdaValue,
    parentSignature: FunctionSignature,
  )(
    implicit registry: Registry,
    registerProvider: RegisterProvider,
    variableRegisterMap: VariableRegisterMap,
    parentCapturedVariableMap: CapturedVariableMap,
  ): (Chunk, Vector[PoemFunction]) = {
    // To generate a lambda function, we have to first perform capture analysis, then compile its body as a
    // single-function multi-function, and finally generate the appropriate `FunctionLambda` instruction.
    val capturedVariables = expression.capturedVariables
    val capturedVariableMap = capturedVariables.zipWithIndex.map { case (variable, index) => variable.uniqueKey -> index }.toMap

    // We can't just map captured variables to registers, because some variables may actually themselves be captured
    // variables, which have to be loaded with `LambdaLocal`. Instead, we have to properly assemble the variables as
    // bindings, just like the ExpressionAssembler does.
    val capturedVariableChunks = capturedVariables.map(TypedTermBindingAssembler.generate)

    // We need to be careful with the name, because any function called `signature.name` can have lambdas which will
    // be global to the VM's universe. For example, we can't simply call these functions `lambda0`, `lambda1`, etc.,
    // because if there are two functions of the same name, their lambda names will clash. As the names are local to
    // the current function, there is no need to have names stable across compilation runs, so a UUID suffices.
    // The `FunctionLambda` instruction passes type arguments to the lambda implicitly, so we have to declare the
    // lambda with the same type parameters. However, we can and should remove the bounds, as these bounds will never
    // be checked.
    val name = parentSignature.name.appendToLastSegment("/lambda-" + UUID.randomUUID().toString)
    val parameters = expression.parameters.map {
      parameter => ParameterDefinition(
        parameter.variable.uniqueKey,
        Some(parameter.variable.name),
        parameter.variable.tpe,
        parameter.position,
      )
    }
    val signature = FunctionSignature(
      name,
      parentSignature.typeParameters.map(_.withoutBounds),
      parameters,
      expression.tpe.output,
      expression.position,
    )
    val generatedPoemFunctions = FunctionAssembler.generate(signature, Some(expression.body), capturedVariableMap)

    // If the lambda has neither type arguments nor captured variables, we can represent it as a constant single
    // function value.
    val regResult = registerProvider.fresh()
    val poemType = TypeAssembler.generate(expression.tpe)
    val instruction = if (signature.isMonomorphic && capturedVariables.isEmpty) {
      PoemInstruction.Const(regResult, PoemSingleFunctionValue(name, Vector.empty, poemType))
    } else {
      PoemInstruction.FunctionLambda(regResult, name, poemType, capturedVariableChunks.map(_.forceResult))
    }
    val resultChunk = Chunk.concat(capturedVariableChunks) ++ Chunk(regResult, instruction)
    (resultChunk, generatedPoemFunctions)
  }

}
