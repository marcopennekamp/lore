package lore.compiler.phases.transpilation

import lore.compiler.Compilation.C
import lore.compiler.ast.{ExprNode, StmtNode}
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.definitions.CallTarget
import lore.compiler.{Compilation, Fragment, Registry}
import lore.compiler.feedback.Error

case class TranspilationNotYetSupported(node: StmtNode)(implicit fragment: Fragment) extends Error(node) {
  override def message = s"The Lore compiler doesn't yet support the transpilation of ${node.getClass.getSimpleName}."
}

/**
  * @param auxiliaryJs Javascript statements that need to be executed before the expression.
  * @param expressionJs A Javascript expression that can be used in an expression position.
  */
case class TranspiledNode(auxiliaryJs: String, expressionJs: String) {
  // TODO: Set expressionJs to None if an expression is Nothing or, in some cases, the unit tuple.
  //       Unit will be tricky, since we sometimes want it as a value and sometimes want to discard it.

  /**
    * Maps the auxiliaryJs and expressionJs to another TranspiledNode.
    */
  def flatMap(f: (String, String) => TranspiledNode): TranspiledNode = f(auxiliaryJs, expressionJs)

  /**
    * Maps the expression part of this TranspiledNode to another expression, keeping the auxiliaryJs
    * consistent.
    */
  def mapExpression(f: String => String): TranspiledNode = {
    TranspiledNode(auxiliaryJs, f(expressionJs))
  }
}

object TranspiledNode {
  def combine(auxiliaryJs: String*)(expression: String): TranspiledNode = TranspiledNode(auxiliaryJs.mkString("\n"), expression)
}

private[transpilation] class FunctionTranspilationVisitor(
  /**
    * The function or constructor which should be transpiled.
    */
  val callTarget: CallTarget,
)(implicit registry: Registry, fragment: Fragment) extends StmtVisitor[TranspiledNode] {
  private var counter = 0
  private def uniqueName(): String = {
    val name = s"tmp$counter"
    counter += 1
    name
  }

  private def default(node: StmtNode): C[TranspiledNode] = Compilation.fail(TranspilationNotYetSupported(node))

  override def visitLeaf(node: StmtNode.LeafNode): Compilation[TranspiledNode] = node match {
    case ExprNode.VariableNode(name) => Compilation.succeed(TranspiledNode("", name))
    case _ => default(node)
  }
  override def visitUnary(node: StmtNode.UnaryNode)(argument: TranspiledNode): Compilation[TranspiledNode] = node match {
    case _ => default(node)
  }
  override def visitBinary(node: StmtNode.BinaryNode)(left: TranspiledNode, right: TranspiledNode): Compilation[TranspiledNode] = node match {
    case ExprNode.LessThanNode(_, _) =>
      val result = left.flatMap { (leftAux, leftExpr) =>
        right.flatMap { (rightAux, rightExpr) =>
          TranspiledNode.combine(leftAux, rightAux)(s"$leftExpr < $rightExpr")
        }
      }
      Compilation.succeed(result)
    case _ => default(node)
  }
  override def visitTernary(node: StmtNode.TernaryNode)(argument1: TranspiledNode, argument2: TranspiledNode, argument3: TranspiledNode): Compilation[TranspiledNode] = node match {
    case ExprNode.IfElseNode(_, onTrue, onFalse) =>
      val result = argument1.flatMap { (conditionAux, condition) =>
        argument2.flatMap { (trueAux, trueExpr) =>
          argument3.flatMap { (falseAux, falseExpr) =>
            val varResult = uniqueName()
            val code =
              s"""$conditionAux
                 |let $varResult;
                 |if ($condition) {
                 |  $trueAux
                 |  ${if (!trueExpr.isBlank) s"$varResult = $trueExpr;" }
                 |} else {
                 |  $falseAux
                 |  ${if (!falseExpr.isBlank) s"$varResult = $falseExpr;" }
                 |}
                 |""".stripMargin
            TranspiledNode(code, varResult)
          }
        }
      }
      Compilation.succeed(result)
    case _ => default(node)
  }
  override def visitXary(node: StmtNode.XaryNode)(arguments: List[TranspiledNode]): Compilation[TranspiledNode] = default(node)
  override def visitMap(node: ExprNode.MapNode)(entries: List[(TranspiledNode, TranspiledNode)]): Compilation[TranspiledNode] = default(node)
  override def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, TranspiledNode)], visitBody: () => Compilation[TranspiledNode]): Compilation[TranspiledNode] = default(node)
}
