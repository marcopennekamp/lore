package lore.compiler.phases.transpilation

import lore.compiler.ast.TopLevelExprNode.AssignmentNode
import lore.compiler.ast.visitor.StmtVisitor
import lore.compiler.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.core.{Compilation, Fragment, Registry}
import lore.compiler.core.feedback.Error
import lore.compiler.functions.FunctionInstance
import lore.compiler.phases.transpilation.Transpilation.Transpilation
import lore.compiler.phases.transpilation.TranspiledChunk.JsCode
import lore.compiler.types.ProductType

case class UnsupportedTranspilation(node: StmtNode) extends Error(node) {
  override def message = s"The Lore compiler doesn't yet support the transpilation of ${node.getClass.getSimpleName}."
}

private[transpilation] class FunctionTranspilationVisitor()(implicit registry: Registry) extends StmtVisitor[TranspiledChunk] {
  import ExprNode._
  import Transpilation.binary

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider

  private def default(node: StmtNode): Transpilation = Compilation.fail(UnsupportedTranspilation(node))

  override def visitLeaf(node: StmtNode.LeafNode): Transpilation = node match {
    case ExprNode.VariableNode(_, state) => Transpilation.expression(state.variable.transpiledName)
    case IntLiteralNode(value, _) => Transpilation.expression(value.toString)
    case RealLiteralNode(value, _) => Transpilation.expression(value.toString)
    case StringLiteralNode(value, _) =>
      // TODO: Escaped characters need to be handled correctly, which they currently are not. For example \'.
      Transpilation.expression(s"'$value'")
    case BoolLiteralNode(value, _) => Transpilation.expression(value.toString)
    case UnitNode(_) => Transpilation.expression(s"${LoreApi.varTuple}.unit")
    case _ => default(node)
  }

  override def visitUnary(node: StmtNode.UnaryNode)(argument: TranspiledChunk): Transpilation = node match {
    case TopLevelExprNode.VariableDeclarationNode(_, isMutable, _, _, state) =>
      val modifier = if (isMutable) "let" else "const"
      val code = s"$modifier ${state.variable.transpiledName} = ${argument.expression.get};"
      Transpilation.statements(argument.statements, code)
    case PropertyAccessNode(_, name, _) =>
      // TODO: This is only a naive implementation which may be temporary. We will ultimately have to ensure that
      //       this works in all cases and perhaps complicate this.
      Compilation.succeed(argument.mapExpression(instance => s"$instance.$name"))
    case NegationNode(_, _) => Compilation.succeed(argument.mapExpression(e => s"-$e"))
    case _ => default(node)
  }

  override def visitBinary(node: StmtNode.BinaryNode)(left: TranspiledChunk, right: TranspiledChunk): Transpilation = node match {
    case AssignmentNode(_, _, _) => binary(left, right, "=")
    case AdditionNode(_, _, _) => binary(left, right, "+", wrap = true)
    case SubtractionNode(_, _, _) => binary(left, right, "-", wrap = true)
    case MultiplicationNode(_, _, _) => binary(left, right, "*", wrap = true)
    case DivisionNode(_, _, _) => binary(left, right, "/", wrap = true)
    case EqualsNode(_, _, _) =>
      // All the complex cases have been filtered already and we can simply apply Javascript comparison.
      binary(left, right, "===", wrap = true)
    case NotEqualsNode(_, _, _) => binary(left, right, "!==", wrap = true)
    case LessThanNode(_, _, _) => binary(left, right, "<", wrap = true)
    case LessThanEqualsNode(_, _, _) => binary(left, right, "<=", wrap = true)
    case GreaterThanNode(_, _, _) => binary(left, right, ">", wrap = true)
    case GreaterThanEqualsNode(_, _, _) => binary(left, right, ">=", wrap = true)
    case node@RepetitionNode(_, _, _) =>
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
    case IfElseNode(_, _, _, _) =>
      val condition = argument1
      val onTrue = argument2
      val onFalse = argument3
      val varResult = nameProvider.createName()
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
      Transpilation.chunk(code, varResult)
    case _ => default(node)
  }

  def transpileCall(targetName: String, arguments: List[TranspiledChunk]): Transpilation = {
    Transpilation.combined(arguments) { expressions =>
      s"$targetName(${expressions.mkString(", ")})"
    }
  }

  def transpileArrayBasedValue(node: StmtNode.XaryNode, createApiFunction: String, expressions: List[TranspiledChunk]): Transpilation = {
    val chunk = RuntimeTypeTranspiler.transpile(node.state.inferredType).flatMap { typeExpression =>
      TranspiledChunk.combined(expressions) { exprs =>
        val values = exprs.mkString(",")
        s"""$createApiFunction(
           |  [$values],
           |  $typeExpression,
           |)""".stripMargin
        // TODO: For the runtime type, don't we need to take types based on the actual values at run-time, not based on
        //       the inferred type???
      }
    }
    Compilation.succeed(chunk)
  }

  override def visitXary(node: StmtNode.XaryNode)(expressions: List[TranspiledChunk]): Transpilation = node match {
    case BlockNode(_, _) => Transpilation.sequencedIdentity(expressions)
    case SimpleCallNode(_, _, _, state) =>
      state.target match {
        case _: FunctionInstance => transpileCall(state.target.name, expressions)
        case _ => default(node) // Constructor calls are not yet supported.
      }
    case DynamicCallNode(_, _, state) =>
      val actualArguments = expressions.tail
      transpileCall(state.target.name, actualArguments)
    case node@TupleNode(_, _) => transpileArrayBasedValue(node, s"${LoreApi.varTuple}.create", expressions)
    case node@ListNode(_, _) => transpileArrayBasedValue(node, s"${LoreApi.varList}.create", expressions)
    case ConcatenationNode(_, _) => Transpilation.operatorChain(expressions, "+", wrap = true)
    case ConjunctionNode(_, _) => Transpilation.operatorChain(expressions, "&&", wrap = true)
    case DisjunctionNode(_, _) => Transpilation.operatorChain(expressions, "||", wrap = true)
    case _ => default(node)
  }

  override def visitMap(node: ExprNode.MapNode)(entries: List[(TranspiledChunk, TranspiledChunk)]): Transpilation = default(node)

  /**
    * Transpiles a loop, combining the scaffolding of loopShell with a correctly transpiled body.
    */
  def transpileLoop(node: LoopNode, loopShell: JsCode => JsCode, body: TranspiledChunk): Transpilation = {
    // TODO: We also need to ignore the resulting list if it isn't used as an expression.
    // The loop's inferred type is Unit if its body type is Unit, so this checks out.
    val ignoreResult = node.state.inferredType == ProductType.UnitType

    def loopCode(varResult: Option[String]): String = {
      val statements = s"${body.statements};"
      val bodyCode = body.expression.map { e =>
        varResult.map { v =>
          s"""$statements
             |${LoreApi.varList}.append($v, $e);
             |""".stripMargin
        } getOrElse {
          s"""$statements
             |$e;""".stripMargin
        }
      } getOrElse statements
      loopShell(bodyCode)
    }

    if (ignoreResult) {
      Transpilation.statements(loopCode(None))
    } else {
      val varResult = nameProvider.createName()
      val typeChunk = RuntimeTypeTranspiler.transpile(node.state.inferredType)
      val resultVarDeclaration =
        s"""const $varResult = ${LoreApi.varList}.create(
           |  [],
           |  ${typeChunk.expression.get},
           |);""".stripMargin
      // TODO: Should we take the run-time type of the elements here or the inferred type? Isn't a list [1] rather
      //       [Int] than [Real], even if it is declared as [Real]?
      //       Oh crap. That would mean we need to LUB types at runtime. Ouch. That's almost impossible with large
      //       lists. We need to put a lot of thought into this... And we can define it either way, so both ways are
      //       possible. Also, what about list concatenation? Fucking hell.
      Transpilation.chunk(List(typeChunk.statements, resultVarDeclaration, loopCode(Some(varResult))), varResult)
    }
  }

  override def visitIteration(node: ExprNode.IterationNode)(
    extractors: List[(String, TranspiledChunk)], visitBody: () => Transpilation
  ): Transpilation = {
    visitBody().flatMap { body =>
      val extractorVariables = node.extractors.map(_.state.variable).map(v => (v.name, v)).toMap
      val loopShell = extractors.map { case (name, chunk) =>
        val variable = extractorVariables(name)
        (inner: String) =>
          s"""${chunk.statements}
             |${LoreApi.varList}.forEach(${chunk.expression.get}, ${variable.transpiledName} => {
             |  $inner
             |});""".stripMargin
      }.foldRight(identity: String => String) { case (enclose, function) => function.andThen(enclose) }
      transpileLoop(node, loopShell, body)
    }
  }
}
