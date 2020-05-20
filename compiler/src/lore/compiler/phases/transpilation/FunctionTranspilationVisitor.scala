package lore.compiler.phases.transpilation

import lore.compiler.Compilation.C
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.definitions.CallTarget
import lore.compiler.feedback.Error
import lore.compiler.types.CompilerSubtyping
import lore.compiler.{Compilation, Fragment, Registry}
import lore.types.{NothingType, ProductType, Type}

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
  def expression(expr: String): TranspiledNode = TranspiledNode("", expr)
  def binary(left: TranspiledNode, right: TranspiledNode)(transform: (String, String) => String): TranspiledNode = {
    left.flatMap { (leftAux, leftExpr) =>
      right.flatMap { (rightAux, rightExpr) =>
        TranspiledNode.combine(leftAux, rightAux)(transform(leftExpr, rightExpr))
      }
    }
  }
}

private[transpilation] class FunctionTranspilationVisitor(
  /**
    * The function or constructor which should be transpiled.
    */
  val callTarget: CallTarget,
)(implicit registry: Registry, fragment: Fragment) extends StmtVisitor[TranspiledNode] {
  import ExprNode._

  private var counter = 0
  private def uniqueName(): String = {
    val name = s"tmp$counter"
    counter += 1
    name
  }

  private def default(node: StmtNode): C[TranspiledNode] = Compilation.fail(TranspilationNotYetSupported(node))
  private def binary(left: TranspiledNode, right: TranspiledNode)(transform: (String, String) => String): Compilation[TranspiledNode] = {
    Compilation.succeed(TranspiledNode.binary(left, right)(transform))
  }

  override def visitLeaf(node: StmtNode.LeafNode): Compilation[TranspiledNode] = node match {
    case ExprNode.VariableNode(name) => Compilation.succeed(TranspiledNode.expression(name))
    case IntLiteralNode(value) => Compilation.succeed(TranspiledNode.expression(value.toString))
    case _ => default(node)
  }
  override def visitUnary(node: StmtNode.UnaryNode)(argument: TranspiledNode): Compilation[TranspiledNode] = node match {
    case TopLevelExprNode.VariableDeclarationNode(name, isMutable, _, _) =>
      val result = argument.flatMap { (aux, expr) =>
        val modifier = if (isMutable) "let" else "const"
        val code = s"$modifier $name = $expr;"
        TranspiledNode.combine(aux, code)("")
      }
      Compilation.succeed(result)
    case _ => default(node)
  }
  override def visitBinary(node: StmtNode.BinaryNode)(left: TranspiledNode, right: TranspiledNode): Compilation[TranspiledNode] = node match {
    case AdditionNode(_, _) => binary(left, right)(_ + " + " + _)
    case SubtractionNode(_, _) => binary(left, right)(_ + " - " + _)
    case MultiplicationNode(_, _) => binary(left, right)(_ + " * " + _)
    case DivisionNode(_, _) => binary(left, right)(_ + " / " + _)
    case EqualsNode(_, _) =>
      // TODO: This can't be a simple equals, of course, unless this is a basic type. We have to implement some kind
      //       of equals function.
      ???
    case NotEqualsNode(_, _) => ???
    case LessThanNode(_, _) => binary(left, right)(_ + " < " + _)
    case LessThanEqualsNode(_, _) => binary(left, right)(_ + " <= " + _)
    case GreaterThanNode(_, _) => binary(left, right)(_ + " > " + _)
    case GreaterThanEqualsNode(_, _) => binary(left, right)(_ + " >= " + _)
    case RepeatWhileNode(condition, body, deferCheck) =>
      val result = left.flatMap { (conditionAux, condition) =>
        right.flatMap { (bodyAux, body) =>
          // TODO: The result variable should rather be a Lore list, not an array.
          val varResult = uniqueName()
          val code =
            s"""const $varResult = [];
               |""".stripMargin
          TranspiledNode(code, varResult)
        }
      }
      ???
    case _ => default(node)
  }
  override def visitTernary(node: StmtNode.TernaryNode)(argument1: TranspiledNode, argument2: TranspiledNode, argument3: TranspiledNode): Compilation[TranspiledNode] = node match {
    case IfElseNode(_, onTrue, onFalse) =>
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

  override def visitXary(node: StmtNode.XaryNode)(expressions: List[TranspiledNode]): Compilation[TranspiledNode] = node match {
    case BlockNode(_) =>
      val expr = expressions.lastOption.map(_.expressionJs).getOrElse("")
      Compilation.succeed(TranspiledNode.combine(expressions.map(_.auxiliaryJs): _*)(expr))
    case TupleNode(_) =>
      ???
    case node@ListNode(_) =>
      val array = expressions.map(_.expressionJs).mkString(", ")
      val expr =
        s"""Values.list(
           |  [$array],
           |  ${RuntimeTypeTranspiler.transpile(node.inferredType)},
           |)""".stripMargin
      Compilation.succeed(TranspiledNode.combine(expressions.map(_.auxiliaryJs): _*)(expr))
    case _ => default(node)
  }

  override def visitMap(node: ExprNode.MapNode)(entries: List[(TranspiledNode, TranspiledNode)]): Compilation[TranspiledNode] = default(node)
  override def visitIteration(node: ExprNode.IterationNode)(extractors: List[(String, TranspiledNode)], visitBody: () => Compilation[TranspiledNode]): Compilation[TranspiledNode] = default(node)
}
