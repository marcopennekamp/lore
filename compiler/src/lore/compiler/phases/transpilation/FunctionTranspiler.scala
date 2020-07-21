package lore.compiler.phases.transpilation

import lore.compiler.CompilerOptions
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.core.Compilation.C
import lore.compiler.core.Registry
import lore.compiler.functions.FunctionDefinition

class FunctionTranspiler(function: FunctionDefinition, uniqueName: String)(implicit compilerOptions: CompilerOptions, registry: Registry) {
  def transpile: C[String] = {
    assert(!function.isAbstract)
    val parameterNames = function.parameters.map(_.asLocalVariable.transpiledName)
    StmtVisitor.visit(new FunctionTranspilationVisitor())(function.body.get).map { chunk =>
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
