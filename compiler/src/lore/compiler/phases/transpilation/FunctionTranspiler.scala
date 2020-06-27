package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.C
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.core.{Fragment, Registry}
import lore.compiler.functions.FunctionDefinition

class FunctionTranspiler(function: FunctionDefinition, uniqueName: String)(implicit registry: Registry) {
  implicit val fragment: Fragment = function.position.fragment

  def transpile: C[String] = {
    assert(!function.isAbstract)
    val parameterNames = function.parameters.map(_.asLocalVariable.transpiledName)
    StmtVisitor.visit(new FunctionTranspilationVisitor())(function.body.get).map { chunk =>
      s"""function $uniqueName(${parameterNames.mkString(", ")}) {
         |  console.info(`Called function $uniqueName with input: (${parameterNames.map(name => "${" + name + "}").mkString(", ")})`);
         |  ${chunk.statements}
         |  ${chunk.expression.map(e => s"return $e;").getOrElse("")}
         |}
         |
         |""".stripMargin
    }
  }
}
