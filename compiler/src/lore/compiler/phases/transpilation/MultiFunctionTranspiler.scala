package lore.compiler.phases.transpilation

import lore.compiler.Compilation
import lore.compiler.Compilation.C
import lore.compiler.definitions.MultiFunctionDefinition

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

      // TODO: Possible optimization when calling functions: Create a hash map of types to functions.
      //       Before going down the multi-function tree, we hash the incoming type and try to pull a
      //       function out of the hash map. If we find a function, it shares the input's exact types,
      //       and is thus the exact function to call. I'm expecting this case to arise a lot, hence
      //       the optimization.
      //       We can someday go a step further and disable this optimization dynamically if we have 50%
      //       or more misses. That is, disabling it on a mf-by-mf basis.

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
