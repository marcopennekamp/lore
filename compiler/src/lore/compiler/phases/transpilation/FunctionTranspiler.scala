package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation
import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.TranspiledChunk.JsCode
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.types.Type

object FunctionTranspiler {

  def transpile(
    function: FunctionDefinition,
    multiFunctionNameProvider: TemporaryNameProvider,
  )(
    implicit compilerOptions: CompilerOptions,
    registry: Registry,
  ): Compilation[(JsCode, RuntimeTypeVariables)] = {
    assert(!function.isAbstract)
    val uniqueName = TranspiledName.function(function)

    // Parameters aren't necessarily only those declared for the function but also the local type variable
    // assignments in case of polymorphic functions.
    var parameterNames = function.signature.parameters.map(_.asLocalVariable.transpiledName)
    if (function.isPolymorphic) {
      parameterNames = TranspiledName.localTypeVariableAssignments +: parameterNames
    }

    // We transpile all type variables here and later use these constants in the multi-function, because the function
    // also potentially needs to have access to its own type variables and this provides better "locality" in the
    // resulting code. Type variables then "belong" to each function that defines them.
    val (typeVariableDefinitions, runtimeTypeVariables) = if (function.isPolymorphic) {
      RuntimeTypeTranspiler.transpileTypeVariables(Type.variables(function.signature.inputType).toList)(multiFunctionNameProvider)
    } else ("", Map.empty: RuntimeTypeVariables)

    ExpressionTranspiler.transpile(function.body.get)(registry, runtimeTypeVariables).map { chunk =>
      val preamble = if (compilerOptions.runtimeLogging) {
        s"console.info(`Called function $uniqueName with input: (${parameterNames.map(name => "${" + name + "}").mkString(", ")})`);"
      } else ""
      val code =
        s"""$typeVariableDefinitions
           |function $uniqueName(${parameterNames.mkString(", ")}) {
           |  $preamble
           |  ${chunk.statements}
           |  ${chunk.expression.map(e => s"return $e;").getOrElse("")}
           |}
           |
           |""".stripMargin
      (code, runtimeTypeVariables)
    }
  }

}
