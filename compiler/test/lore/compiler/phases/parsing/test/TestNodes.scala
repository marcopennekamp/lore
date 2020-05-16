package lore.compiler.phases.parsing.test

import lore.compiler.ast.ExprNode.VariableNode
import lore.compiler.ast.TypeExprNode.NominalNode

object TestNodes {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val tReal = NominalNode("Real")
  val tInt = NominalNode("Int")
  val tString = NominalNode("String")
  val tBoolean = NominalNode("Boolean")

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val va = VariableNode("a")
  val vb = VariableNode("b")
  val vc = VariableNode("c")
  val vx = VariableNode("x")
  val vy = VariableNode("y")
  val vz = VariableNode("z")
  val vi = VariableNode("i")
  val vk = VariableNode("k")
}
