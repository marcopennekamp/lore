package lore.compiler.assembly.functions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem.{PoemFunction, PoemInstruction, PoemSingleFunctionValue}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression.AnonymousFunction
import lore.compiler.semantics.functions.{FunctionSignature, ParameterDefinition}

import java.util.UUID

object LambdaAssembler {

  def generate(
    expression: AnonymousFunction,
    parentSignature: FunctionSignature,
  )(
    implicit registry: Registry,
    registerProvider: RegisterProvider,
    variableRegisterMap: VariableRegisterMap,
  ): (AsmChunk, Vector[PoemFunction]) = {
    // To generate an anonymous function, we have to first perform capture analysis, then compile its body as a
    // single-function multi-function, and finally generate the appropriate `Lambda` instruction.
    val capturedVariables = expression.capturedVariables
    val capturedVariableMap = capturedVariables.zipWithIndex.map { case (variable, index) => variable.uniqueKey -> index }.toMap
    val capturedRegisters = capturedVariables.map(variable => variableRegisterMap(variable.uniqueKey))

    // We need to be careful with the name, because any function called `signature.name` can have lambdas which will
    // be global to the VM's universe. For example, we can't simply call these functions `lambda0`, `lambda1`, etc.,
    // because if there are two functions of the same name, their lambda names will clash. As the names are local to
    // the current function, there is no need to have names stable across compilation runs, so a UUID suffices.
    // The `Lambda` instruction passes type arguments to the lambda implicitly, so we have to declare the lambda with
    // the same type parameters. However, we can and should remove the bounds, as these bounds will never be checked.
    val name = parentSignature.name.appendToLastSegment("/lambda-" + UUID.randomUUID().toString)
    val parameters = expression.parameters.map {
      parameter => ParameterDefinition(parameter.uniqueKey, Some(parameter.name), parameter.tpe, parameter.position)
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
      PoemInstruction.Lambda(regResult, name, poemType, capturedRegisters)
    }
    (AsmChunk(regResult, instruction), generatedPoemFunctions)
  }

}
