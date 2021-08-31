package lore.compiler.transpilation.functions

import lore.compiler.core.{CompilationException, CompilerOptions}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.target.Target.{TargetName, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation._
import lore.compiler.transpilation.expressions.ExpressionTranspiler
import lore.compiler.transpilation.values.SymbolHistory

object FunctionTranspiler {

  def transpile(function: FunctionDefinition)(
    implicit compilerOptions: CompilerOptions,
    registry: Registry,
    runtimeTypeVariables: RuntimeTypeVariables,
    symbolHistory: SymbolHistory,
    variableProvider: TemporaryVariableProvider,
  ): Vector[TargetStatement] = {
    transpile(function, RuntimeNames.functionDefinition(function).name, shouldExport = false)
  }

  def transpile(
    function: FunctionDefinition,
    name: TargetName,
    shouldExport: Boolean,
  )(
    implicit compilerOptions: CompilerOptions,
    registry: Registry,
    runtimeTypeVariables: RuntimeTypeVariables,
    symbolHistory: SymbolHistory,
    variableProvider: TemporaryVariableProvider,
  ): Vector[TargetStatement] = {
    if (function.isAbstract) {
      throw CompilationException(s"Cannot transpile abstract function $function.")
    }

    // We have to give unnamed parameters temporary variable names to preserve the function's arity.
    var transpiledParameters = function.signature.parameters.map { parameter =>
      parameter.name match {
        case Some(name) => RuntimeNames.localVariable(name).asParameter
        case None => variableProvider.createVariable().asParameter
      }
    }

    // Parameters aren't necessarily only those declared for the function but also the local type variable
    // assignments in case of polymorphic functions.
    if (function.isPolymorphic) {
      transpiledParameters = RuntimeNames.localTypeVariableAssignments.asParameter +: transpiledParameters
    }

    val chunk = ExpressionTranspiler.transpile(function.body.get)

    val preamble = if (compilerOptions.enableRuntimeLogging) {
      val argumentValues = transpiledParameters.flatMap(parameter => Vector(parameter.asVariable, ", ".asLiteral))
      RuntimeApi.io.println(
        Target.Operation(
          TargetOperator.Concat,
          Vector(s"Called function $name with input: (".asLiteral) ++ argumentValues ++ Vector(")".asLiteral),
        ),
      )
    } else Target.Empty

    Vector(Target.Function(
      name,
      transpiledParameters,
      Chunk(Vector(preamble) ++ chunk.statements, chunk.expression).asBody,
      shouldExport
    ))
  }

}
