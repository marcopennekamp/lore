package lore.compiler.phases.transpilation

import lore.compiler.ast.TopLevelExprNode.AssignmentNode
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.{Compilation, Fragment, Registry}
import lore.compiler.definitions.{FunctionDefinition, InternalCallTarget}
import lore.compiler.feedback.Error
import lore.compiler.phases.transpilation.Transpilation.Transpilation
import lore.compiler.phases.transpilation.TranspiledChunk.JsCode

case class UnsupportedTranspilation(node: StmtNode)(implicit fragment: Fragment) extends Error(node) {
  override def message = s"The Lore compiler doesn't yet support the transpilation of ${node.getClass.getSimpleName}."
}

private[transpilation] class FunctionTranspilationVisitor(
  /**
    * The function or constructor which should be transpiled.
    */
  val callTarget: InternalCallTarget,
)(implicit registry: Registry, fragment: Fragment) extends StmtVisitor[TranspiledChunk] {
  import ExprNode._
  import Transpilation.binary

  private var uniqueNameCounter = 0
  private def uniqueName(): String = {
    val name = s"tmp$uniqueNameCounter"
    uniqueNameCounter += 1
    name
  }

  private def default(node: StmtNode): Transpilation = Compilation.fail(UnsupportedTranspilation(node))

  override def visitLeaf(node: StmtNode.LeafNode): Transpilation = node match {
    case ExprNode.VariableNode(name) => Transpilation.expression(name)
    case IntLiteralNode(value) => Transpilation.expression(value.toString)
    case StringLiteralNode(value) => Transpilation.expression(s"'$value'")
    case UnitNode => Transpilation.expression(s"${LoreApi.varValues}.unit")
    case _ => default(node)
  }

  override def visitUnary(node: StmtNode.UnaryNode)(argument: TranspiledChunk): Transpilation = node match {
    case TopLevelExprNode.VariableDeclarationNode(name, isMutable, _, _) =>
      val modifier = if (isMutable) "let" else "const"
      val code = s"$modifier $name = ${argument.expression.get};"
      Transpilation.statements(argument.statements, code)
    case NegationNode(_) => Compilation.succeed(argument.mapExpression(e => s"-$e"))
    case _ => default(node)
  }

  override def visitBinary(node: StmtNode.BinaryNode)(left: TranspiledChunk, right: TranspiledChunk): Transpilation = node match {
    case AssignmentNode(_, _) => binary(left, right, "=")
    case AdditionNode(_, _) => binary(left, right, "+")
    case SubtractionNode(_, _) => binary(left, right, "-")
    case MultiplicationNode(_, _) => binary(left, right, "*")
    case DivisionNode(_, _) => binary(left, right, "/")
    case EqualsNode(_, _) =>
      // TODO: This can't be a simple equals, of course, unless this is a basic type. We have to implement some kind
      //       of equals function.
      default(node)
    case NotEqualsNode(_, _) => default(node)
    case LessThanNode(_, _) => binary(left, right, "<")
    case LessThanEqualsNode(_, _) => binary(left, right, "<=")
    case GreaterThanNode(_, _) => binary(left, right, ">")
    case GreaterThanEqualsNode(_, _) => binary(left, right, ">=")
    case node@RepetitionNode(_, _) =>
      val condition = left
      val body = right
      val loopShell = (bodyCode: String) => {
        s"""${condition.statements}
           |while (${condition.expression.get}) {
           |  $bodyCode
           |}
           |""".stripMargin
      }
      transpileLoop(node, loopShell, body)
    case _ => default(node)
  }

  override def visitTernary(node: StmtNode.TernaryNode)(
    argument1: TranspiledChunk, argument2: TranspiledChunk, argument3: TranspiledChunk
  ): Transpilation = node match {
    case IfElseNode(_, _, _) =>
      val condition = argument1
      val onTrue = argument2
      val onFalse = argument3
      val varResult = uniqueName()
      val code =
        s"""${condition.statements}
           |let $varResult;
           |if (${condition.expression.get}) {
           |  ${onTrue.statements}
           |  ${onTrue.expression.map(e => s"$varResult = $e;").getOrElse("")}
           |} else {
           |  ${onFalse.statements}
           |  ${onFalse.expression.map(e => s"$varResult = $e;").getOrElse("")}
           |}
           |""".stripMargin
      Transpilation.chunk(code, Some(varResult))
    case _ => default(node)
  }

  def transpileCall(targetName: String, arguments: List[TranspiledChunk]): Transpilation = {
    Transpilation.combined(arguments) { expressions =>
      s"$targetName(${expressions.mkString(", ")})"
    }
  }

  def transpileArrayBasedValue(node: StmtNode.XaryNode, valuesApiFunction: String, expressions: List[TranspiledChunk]): Transpilation = {
    Transpilation.combined(expressions) { exprs =>
      val values = exprs.mkString(",")
      s"""${LoreApi.varValues}.$valuesApiFunction(
         |  [$values],
         |  ${RuntimeTypeTranspiler.transpile(node.inferredType)},
         |)""".stripMargin
    }
  }

  override def visitXary(node: StmtNode.XaryNode)(expressions: List[TranspiledChunk]): Transpilation = node match {
    case BlockNode(_) => Transpilation.sequencedIdentity(expressions)
    case node@SimpleCallNode(_, _, _) =>
      node.target match {
        case _: FunctionDefinition => transpileCall(node.target.name, expressions)
        case _ => default(node) // Constructor calls are not yet supported.
      }
    case node@DynamicCallNode(_, _) =>
      val actualArguments = expressions.tail
      transpileCall(node.target.name, actualArguments)
    case node@TupleNode(_) => transpileArrayBasedValue(node, "tuple", expressions)
    case node@ListNode(_) => transpileArrayBasedValue(node, "list", expressions)
    case ConcatenationNode(_) => Transpilation.operatorChain(expressions, "+")
    case _ => default(node)
  }

  override def visitMap(node: ExprNode.MapNode)(entries: List[(TranspiledChunk, TranspiledChunk)]): Transpilation = default(node)

  /**
    * Transpiles a loop, combining the scaffolding of loopShell with a correctly transpiled body.
    */
  def transpileLoop(node: LoopNode, loopShell: JsCode => JsCode, body: TranspiledChunk): Transpilation = {
    val varResult = uniqueName()
    val bodyCode =
      s"""${body.statements}
         |${body.expression.map(e => s"$varResult.push($e);").getOrElse("")}
         |""".stripMargin
    val loopCode = loopShell(bodyCode)
    // TODO: This needs to be optimized away if the resulting list isn't used as an expression.
    val resultVarDeclaration =
      s"""const $varResult = ${LoreApi.varValues}.list(
         |  [],
         |  ${RuntimeTypeTranspiler.transpile(node.inferredType)},
         |);""".stripMargin
    val code =
      s"""$resultVarDeclaration
         |$loopCode
         |""".stripMargin
    Transpilation.chunk(code, Some(varResult))
  }

  override def visitIteration(node: ExprNode.IterationNode)(
    extractors: List[(String, TranspiledChunk)], visitBody: () => Transpilation
  ): Transpilation = {
    visitBody().flatMap { body =>
      val loopShell = extractors.map { case (name, node) =>
        (inner: String) =>
          s"""${node.statements}
             |${node.expression.get}.forEach($name => {
             |  $inner
             |});""".stripMargin
      }.foldRight(identity: String => String) { case (enclose, function) => function.andThen(enclose) }
      transpileLoop(node, loopShell, body)
    }
  }
}
