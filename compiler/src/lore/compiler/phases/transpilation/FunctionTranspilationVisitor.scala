package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.transpilation.Transpilation.Transpilation
import lore.compiler.phases.transpilation.TranspiledChunk.JsCode
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.{DynamicCallTarget, FunctionInstance}
import lore.compiler.types.{BasicType, ListType, MapType, ProductType}

case class UnsupportedTranspilation(expression: Expression) extends Error(expression) {
  override def message = s"The Lore compiler doesn't yet support the transpilation of ${expression.getClass.getSimpleName}."
}

private[transpilation] class FunctionTranspilationVisitor()(implicit registry: Registry) extends ExpressionVisitor[TranspiledChunk] {
  import Expression._

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider

  private def default(expression: Expression): Transpilation = Compilation.fail(UnsupportedTranspilation(expression))

  override def visit(expression: Return)(value: TranspiledChunk): Transpilation = {
    val ret = s"return ${value.expression.get};"
    Transpilation.chunk(value.statements :: ret :: Nil, RuntimeApi.values.tuple.unit)
  }

  override def visit(expression: VariableDeclaration)(value: TranspiledChunk): Transpilation = {
    val variable = expression.variable
    val modifier = if (variable.isMutable) "let" else "const"
    val code = s"$modifier ${variable.transpiledName} = ${value.expression.get};"
    Transpilation.statements(value.statements, code)
  }

  override def visit(expression: Assignment)(target: TranspiledChunk, value: TranspiledChunk): Transpilation = {
    Transpilation.binary(target, value, "=", wrap = false)
  }

  override def visit(expression: Construct)(arguments: List[TranspiledChunk], superCall: Option[TranspiledChunk]): Transpilation = default(expression)

  override def visit(expression: Block)(expressions: List[TranspiledChunk]): Transpilation = Transpilation.sequencedIdentity(expressions)

  override def visit(expression: VariableAccess): Transpilation = Transpilation.expression(expression.variable.transpiledName)

  override def visit(expression: MemberAccess)(instance: TranspiledChunk): Transpilation = {
    // TODO: This is only a naive implementation which may be temporary. We will ultimately have to ensure that
    //       this works in all cases and perhaps complicate this.
    instance.mapExpression(instance => s"$instance.${expression.member.name}").compiled
  }

  override def visit(literal: Literal): Transpilation = literal.tpe match {
    case BasicType.Real | BasicType.Int | BasicType.Boolean => Transpilation.expression(literal.value.toString)
    case BasicType.String =>
      // TODO: Escaped characters need to be handled correctly, which they currently are not. For example \'.
      Transpilation.expression(s"'${literal.value}'")
  }

  override def visit(expression: Tuple)(values: List[TranspiledChunk]): Transpilation = {
    if (expression.tpe == ProductType.UnitType) {
      Transpilation.expression(RuntimeApi.values.tuple.unit)
    } else {
      transpileArrayBasedValue(expression, RuntimeApi.values.tuple.create, values)
    }
  }

  override def visit(expression: ListConstruction)(values: List[TranspiledChunk]): Transpilation = {
    transpileArrayBasedValue(expression, RuntimeApi.values.list.create, values)
  }

  override def visit(expression: MapConstruction)(entries: List[(TranspiledChunk, TranspiledChunk)]): Transpilation = {
    val entryChunks = entries.map { case (keyChunk, valueChunk) =>
      TranspiledChunk.combined(List(keyChunk, valueChunk)) {
        case List(keyExpr, valueExpr) => s"[$keyExpr, $valueExpr]"
      }
    }
    // The extra arguments are passed to the create function of the map and represent the hash and equals methods used
    // by the hash map. We cannot reference these from the runtime because we can't import them there!
    transpileArrayBasedValue(expression, RuntimeApi.values.map.create, entryChunks, extra = "hash, areEqual,")
  }

  override def visit(expression: UnaryOperation)(value: TranspiledChunk): Transpilation = {
    val operatorString = expression.operator match {
      case UnaryOperator.Negation => "-"
      case UnaryOperator.LogicalNot => "!"
    }
    value.mapExpression(e => s"$operatorString$e").compiled
  }

  override def visit(expression: BinaryOperation)(left: TranspiledChunk, right: TranspiledChunk): Transpilation = {
    val operatorString = expression.operator match {
      case BinaryOperator.Addition => "+"
      case BinaryOperator.Subtraction => "-"
      case BinaryOperator.Multiplication => "*"
      case BinaryOperator.Division => "/"
      // All the complex cases have been filtered already and we can simply apply Javascript comparison.
      case BinaryOperator.Equals => "==="
      case BinaryOperator.LessThan => "<"
      case BinaryOperator.LessThanEquals => "<="
    }
    Transpilation.binary(left, right, operatorString)
  }

  override def visit(expression: XaryOperation)(operands: List[TranspiledChunk]): Transpilation = {
    val operatorString = expression.operator match {
      case XaryOperator.Conjunction => "&&"
      case XaryOperator.Disjunction => "||"
      case XaryOperator.Concatenation => "+"
    }
    Transpilation.operatorChain(operands, operatorString)
  }

  override def visit(expression: Call)(arguments: List[TranspiledChunk]): Transpilation = {
    expression.target match {
      case _: FunctionInstance | _: DynamicCallTarget => transpileCall(expression.target.name, arguments)
      case _ => default(expression) // Constructor calls are not yet supported.
    }
  }

  override def visit(expression: IfElse)(condition: TranspiledChunk, onTrue: TranspiledChunk, onFalse: TranspiledChunk): Transpilation = {
    // TODO: If the result is Unit there should be no varResult and no expression result.
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
  }

  override def visit(loop: WhileLoop)(condition: TranspiledChunk, body: TranspiledChunk): Transpilation = {
    val loopShell = (bodyCode: String) => {
      s"""${condition.statements}
         |while (${condition.expression.get}) {
         |  $bodyCode
         |}
         |""".stripMargin
    }
    transpileLoop(loop, loopShell, body)
  }

  override def visit(loop: ForLoop)(extractors: List[TranspiledChunk], body: TranspiledChunk): Transpilation = {
    val loopShell = loop.extractors.zip(extractors).map {
      case (extractor, chunk) =>
        val forEach = extractor.collection.tpe match {
          case ListType(_) => RuntimeApi.values.list.forEach
          case MapType(_, _) => RuntimeApi.values.map.forEach
          case _ => throw CompilationException("Only lists and maps can be used as collections in a for loop.")
        }
        (inner: String) =>
          s"""${chunk.statements}
             |$forEach(${chunk.expression.get}, ${extractor.variable.transpiledName} => {
             |  $inner
             |});""".stripMargin
    }.foldRight(identity: String => String) { case (enclose, function) => function.andThen(enclose) }
    transpileLoop(loop, loopShell, body)
  }

  /**
    * Transpiles a loop, combining the scaffolding of loopShell with a correctly transpiled body.
    */
  private def transpileLoop(loop: Loop, loopShell: JsCode => JsCode, body: TranspiledChunk): Transpilation = {
    // TODO: We also need to ignore the resulting list if it isn't used as an expression.
    // The loop's inferred type is Unit if its body type is Unit, so this checks out.
    val ignoreResult = loop.tpe == ProductType.UnitType

    def loopCode(varResult: Option[String]): String = {
      val statements = s"${body.statements};"
      val bodyCode = body.expression.map { e =>
        varResult.map { v =>
          s"""$statements
             |${RuntimeApi.values.list.append}($v, $e);
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
      val typeChunk = RuntimeTypeTranspiler.transpile(loop.tpe)
      val resultVarDeclaration =
        s"""const $varResult = ${RuntimeApi.values.list.create}(
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

  /**
    * Transpiles a call with the given target name and arguments.
    */
  private def transpileCall(targetName: String, arguments: List[TranspiledChunk]): Transpilation = {
    // TODO: Rather take the target as an argument?
    Transpilation.combined(arguments) { expressions =>
      s"$targetName(${expressions.mkString(", ")})"
    }
  }

  /**
    * Transpiles a xary-constructed value that is created using an API function wrapped around an array of parts.
    */
  private def transpileArrayBasedValue(
    expression: Expression, createApiFunction: String, values: List[TranspiledChunk], extra: String = "",
  ): Transpilation = {
    val chunk = RuntimeTypeTranspiler.transpile(expression.tpe).flatMap { typeExpression =>
      TranspiledChunk.combined(values) { values =>
        s"""$createApiFunction(
           |  [${values.mkString(",")}],
           |  $typeExpression,
           |  $extra
           |)""".stripMargin
        // TODO: For the runtime type, don't we need to take types based on the actual values at run-time, not based on
        //       the inferred type??? This is a big problem for lists and such.
      }
    }
    chunk.compiled
  }
}
