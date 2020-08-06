package lore.compiler.phases.transpilation

import lore.compiler.semantics.structures.ClassDefinition

/**
  * Transpiles the class definition. It relies on the class type already being registered.
  */
class ClassDefinitionTranspiler(definition: ClassDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry) {
  def transpile: Compilation[String] = {
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
