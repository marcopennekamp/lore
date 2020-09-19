package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.Transpilation.Transpilation
import lore.compiler.phases.transpilation.TranspiledChunk.{JsCode, JsExpr}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.{DynamicCallTarget, FunctionInstance}
import lore.compiler.types.{BasicType, ListType, MapType, ProductType, Type}

case class UnsupportedTranspilation(expression: Expression) extends Error(expression) {
  override def message = s"The Lore compiler doesn't yet support the transpilation of ${expression.getClass.getSimpleName}."
}

private[transpilation] class FunctionTranspilationVisitor()(
  implicit registry: Registry, runtimeTypeVariables: RuntimeTypeVariables
) extends ExpressionVisitor[TranspiledChunk] {
  import Expression._

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider

  private def default(expression: Expression): Transpilation = Compilation.fail(UnsupportedTranspilation(expression))

  override def visit(expression: Return)(value: TranspiledChunk): Transpilation = {
    val ret = s"return ${value.expression.get};"
    Transpilation.chunk(Vector(value.statements, ret), RuntimeApi.values.tuple.unit)
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

  override def visit(expression: Block)(expressions: Vector[TranspiledChunk]): Transpilation = Transpilation.sequencedIdentity(expressions)

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

  override def visit(expression: Tuple)(values: Vector[TranspiledChunk]): Transpilation = {
    if (expression.tpe == ProductType.UnitType) {
      Transpilation.expression(RuntimeApi.values.tuple.unit)
    } else {
      Transpilation.combined(values) { values =>
        s"${RuntimeApi.values.tuple.create}([${values.mkString(",")}])"
      }
    }
  }

  override def visit(expression: ListConstruction)(values: Vector[TranspiledChunk]): Transpilation = {
    transpileArrayBasedValue(expression, RuntimeApi.values.list.create, values)
  }

  override def visit(expression: MapConstruction)(entries: Vector[(TranspiledChunk, TranspiledChunk)]): Transpilation = {
    val entryChunks = entries.map { case (keyChunk, valueChunk) =>
      TranspiledChunk.combined(Vector(keyChunk, valueChunk)) {
        case Vector(keyExpr, valueExpr) => s"[$keyExpr, $valueExpr]"
      }
    }
    // The extra arguments are passed to the create function of the map and represent the hash and equals methods used
    // by the hash map. We cannot reference these from the runtime because we can't import them there!
    transpileArrayBasedValue(expression, RuntimeApi.values.map.create, entryChunks, extra = "hash, areEqual,")
  }

  override def visit(expression: Instantiation)(arguments: Vector[TranspiledChunk]): Transpilation = {
    Transpilation.combined(arguments) { argumentJsExprs =>
      // TODO: To properly support components, we will have to put the types of the actual components into the object
      //       type. This goes hand-in-hand with the component type TODO in DeclaredTypeTranspiler.
      val members = expression.arguments.map(_.member)
      val objectProperties = members.zip(argumentJsExprs).map { case (member, jsExpr) => s"${member.name}: $jsExpr" }
      val varType = TranspiledNames.declaredType(expression.struct.tpe)
      s"${RuntimeApi.values.`object`.create}({ ${objectProperties.mkString(",")} }, $varType)"
    }
  }

  override def visit(expression: UnaryOperation)(value: TranspiledChunk): Transpilation = {
    val operatorString = expression.operator match {
      case UnaryOperator.Negation => "-"
      case UnaryOperator.LogicalNot => "!"
    }
    value.mapExpression(e => s"$operatorString$e").compiled
  }

  override def visit(expression: BinaryOperation)(left: TranspiledChunk, right: TranspiledChunk): Transpilation = {
    // Filter those cases first that can't simply be translated to a binary Javascript operator.
    expression.operator match {
      case BinaryOperator.Append => transpileListAppends(left, right, expression.tpe)
      case _ =>
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
  }

  override def visit(expression: XaryOperation)(operands: Vector[TranspiledChunk]): Transpilation = {
    val operatorString = expression.operator match {
      case XaryOperator.Conjunction => "&&"
      case XaryOperator.Disjunction => "||"
      case XaryOperator.Concatenation => "+"
    }
    Transpilation.operatorChain(operands, operatorString)
  }

  override def visit(expression: Call)(arguments: Vector[TranspiledChunk]): Transpilation = {
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

  /**
    * Transpile a for-loop. We use direct iteration for this instead of a more succinct forEach because we need to
    * be able to return from within the loop.
    */
  override def visit(loop: ForLoop)(extractors: Vector[TranspiledChunk], body: TranspiledChunk): Transpilation = {
    val loopShell = loop.extractors.zip(extractors).map {
      case (extractor, chunk) =>
        (inner: String) => extractor.collection.tpe match {
          case ListType(_) =>
            val varList = nameProvider.createName()
            s"""${chunk.statements}
               |const $varList = ${chunk.expression.get};
               |for (let i = 0; i < $varList.array.length; i += 1) {
               |  const ${extractor.variable.transpiledName} = $varList.array[i];
               |  $inner
               |}""".stripMargin
          case MapType(_, _) =>
            val varIterator = nameProvider.createName()
            val varResult = nameProvider.createName()
            s"""${chunk.statements}
               |const $varIterator = ${RuntimeApi.values.map.entries}(${chunk.expression.get});
               |let $varResult = $varIterator.next();
               |while(!$varResult.done) {
               |  const ${extractor.variable.transpiledName} = $varResult.value;
               |  $inner
               |  $varResult = $varIterator.next();
               |}""".stripMargin
          case _ => throw CompilationException("Currently, only lists and maps can be used as collections in a for loop.")
        }
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

    def loopCode(varResult: Option[String], resultType: JsExpr = ""): String = {
      val statements = s"${body.statements};"
      val bodyCode = body.expression.map { e =>
        varResult.map { v =>
          s"""$statements
             |$v = ${RuntimeApi.values.list.append}($v, $e, $resultType);
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
      val resultType = RuntimeTypeTranspiler.transpileSubstitute(loop.tpe)
      val resultVarDeclaration =
        s"""let $varResult = ${RuntimeApi.values.list.create}(
           |  [],
           |  $resultType,
           |);""".stripMargin
      Transpilation.chunk(Vector(resultVarDeclaration, loopCode(Some(varResult), resultType)), varResult)
    }
  }

  /**
    * Transpiles a call with the given target name and arguments.
    */
  private def transpileCall(targetName: String, arguments: Vector[TranspiledChunk]): Transpilation = {
    // TODO: Rather take the target as an argument?
    Transpilation.combined(arguments) { expressions =>
      s"$targetName(${expressions.mkString(", ")})"
    }
  }

  /**
    * Transpiles a xary-constructed value that is created using an API function wrapped around an array of parts.
    */
  private def transpileArrayBasedValue(
    expression: Expression, createApiFunction: String, values: Vector[TranspiledChunk], extra: String = "",
  ): Transpilation = {
    val typeExpr = RuntimeTypeTranspiler.transpileSubstitute(expression.tpe)
    Transpilation.combined(values) { valueExprs =>
      s"""$createApiFunction(
         |  [${valueExprs.mkString(",")}],
         |  $typeExpr,
         |  $extra
         |)""".stripMargin
    }
  }

  private def transpileListAppends(list: TranspiledChunk, element: TranspiledChunk, resultType: Type): Transpilation = {
    // TODO: We could also translate the append operation to a dynamic function call in the FunctionTransformationVisitor.
    //       However, we will have to support passing types as expressions, at least for the compiler, because the
    //       last argument to 'append' has to be the new list type.
    // TODO: This type transpilation is not quite correct for type variables. Assuming the appends happens in a
    //       function context, we will have to take the actual type assigned to the type variable during this
    //       specific function call (at run-time) from a sort of type context that gets populated in polymorphic
    //       functions. Because if we have a function over a list [T] and an element T and we call the function
    //       with, say, T = Int, we don't want the resulting list type to be [T] but rather [Int].
    val typeExpr = RuntimeTypeTranspiler.transpileSubstitute(resultType)
    Transpilation.combined(Vector(list, element)) { case Vector(listExpr, elementExpr) =>
      s"${RuntimeApi.values.list.append}($listExpr, $elementExpr, $typeExpr)"
    }
  }
}
