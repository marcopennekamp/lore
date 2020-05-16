package lore.compiler.phases.transpilation

import lore.compiler.Compilation
import lore.compiler.Compilation.C
import lore.definitions.MultiFunctionDefinition

object MultiFunctionTranspiler {
  def transpile(mf: MultiFunctionDefinition): C[String] = {
    val result = new StringBuilder()

    // We transpile the functions by arity, because functions with different arities are inherently incompatible
    // with each other, and thus don't need to be considered as possible pairings to inputs of a different arity
    // in the call tree.
    val arityGroups = mf.functions.groupBy(_.parameters.length)

    arityGroups.map { case (arity, group) =>
      group.zipWithIndex.foreach { case (function, index) =>
        val name = s"${function.name}$$${arity}_$index"
        val parameters = function.parameters.map(_.name).mkString(", ")
        result.append(
          s"""function $name($parameters) {
             |  console.log('Called function $name');
             |}
             |
             |""".stripMargin
        )
      }

      val mfName = s"${mf.name}$$$arity"
      val parameters = (0 until arity).map(n => s"arg$n").mkString(", ")
      result.append(
        s"""function $mfName($parameters) {
           |  console.log('Called multi-function $mfName');
           |}
           |""".stripMargin
      )
    }

    Compilation.succeed(result.toString)
  }
}
