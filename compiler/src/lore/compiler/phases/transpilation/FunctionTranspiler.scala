package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.ExpressionVisitor
import lore.compiler.semantics.functions.FunctionDefinition

object FunctionTranspiler {
  def transpile(
    function: FunctionDefinition, uniqueName: String
  )(implicit compilerOptions: CompilerOptions, registry: Registry): Compilation[String] = {
    assert(!function.isAbstract)
    val parameterNames = function.parameters.map(_.asLocalVariable.transpiledName)
    ExpressionVisitor.visit(new FunctionTranspilationVisitor())(function.body.get).map { chunk =>
      val preamble = if (compilerOptions.runtimeLogging) {
        s"console.info(`Called function $uniqueName with input: (${parameterNames.map(name => "${" + name + "}").mkString(", ")})`);"
      } else ""
      s"""function $uniqueName(${parameterNames.mkString(", ")}) {
         |  $preamble
         |  ${chunk.statements}
         |  ${chunk.expression.map(e => s"return $e;").getOrElse("")}
         |}
         |
         |""".stripMargin
    }
  }
}
