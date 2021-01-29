package lore.compiler.phases.transpilation.functions

import lore.compiler.CompilerOptions
import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.TranspiledTypeVariables
import lore.compiler.phases.transpilation._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}

object FunctionTranspiler {

  def transpile(function: FunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry, typeVariables: TranspiledTypeVariables): Vector[TargetStatement] = {
    if (function.isAbstract) {
      throw CompilationException(s"Cannot transpile abstract function $function.")
    }
    val uniqueName = TranspiledName.function(function)

    // Parameters aren't necessarily only those declared for the function but also the local type variable
    // assignments in case of polymorphic functions.
    var transpiledParameters = function.signature.parameters.map(_.asLocalVariable.transpiledName.asParameter)
    if (function.isPolymorphic) {
      transpiledParameters = TranspiledName.localTypeVariableAssignments.asParameter +: transpiledParameters
    }

    val chunk = ExpressionTranspiler.transpile(function.body.get)

    val preamble = if (compilerOptions.runtimeLogging) {
      val argumentValues = transpiledParameters.flatMap(parameter => Vector(parameter.asVariable, ", ".asLiteral))
      RuntimeApi.io.println(
        Target.Operation(
          TargetOperator.Concat,
          Vector(s"Called function $uniqueName with input: (".asLiteral) ++ argumentValues ++ Vector(")".asLiteral)
        )
      )
    } else Target.Empty

    Vector(Target.Function(
      uniqueName,
      transpiledParameters,
      Chunk(Vector(preamble) ++ chunk.statements, chunk.expression).asBody
    ))
  }

}
