package lore.compiler.phases.transpilation

import lore.compiler.Compilation.C
import lore.compiler.{Fragment, Registry}
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.definitions.FunctionDefinition

class FunctionTranspiler(function: FunctionDefinition, uniqueName: String)(implicit registry: Registry) {
  implicit val fragment: Fragment = function.position.fragment

  def transpile: C[String] = {
    assert(!function.isAbstract)
    val parameters = function.parameters.map(_.name).mkString(", ")
    StmtVisitor.visit(new FunctionTranspilationVisitor(function))(function.body.get).map {
      case TranspiledNode(auxiliary, expr) =>
        s"""function $uniqueName($parameters) {
           |  console.log('Called function $uniqueName');
           |  ${function.parameters.map(p => s"console.log(${p.name});").mkString("\n")}
           |  $auxiliary
           |  ${if (!expr.isBlank) s"return $expr;" }
           |}
           |
           |""".stripMargin
    }
  }
}
