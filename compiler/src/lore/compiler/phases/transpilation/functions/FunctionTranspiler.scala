package lore.compiler.phases.transpilation.functions

import lore.compiler.core.{CompilationException, CompilerOptions}
import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation._
import lore.compiler.phases.transpilation.expressions.ExpressionTranspiler
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.target.Target.{TargetName, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}

object FunctionTranspiler {

  def transpile(function: FunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry, runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    transpile(function, function.targetVariable.name, shouldExport = false)
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
  ): Vector[TargetStatement] = {
    if (function.isAbstract) {
      throw CompilationException(s"Cannot transpile abstract function $function.")
    }

    // Parameters aren't necessarily only those declared for the function but also the local type variable
    // assignments in case of polymorphic functions.
    var transpiledParameters = function.signature.parameters.map(_.asTargetParameter)
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
