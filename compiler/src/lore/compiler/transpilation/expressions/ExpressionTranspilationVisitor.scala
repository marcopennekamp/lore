
package lore.compiler.transpilation.expressions

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation._
import lore.compiler.transpilation.structures.InstantiationTranspiler
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.types._

private[transpilation] class ExpressionTranspilationVisitor()(
  implicit registry: Registry,
  runtimeTypeVariables: RuntimeTypeVariables,
  symbolHistory: SymbolHistory,
) extends ExpressionVisitor[Chunk, Chunk] {
  import Expression._

  private implicit val variableProvider: TemporaryVariableProvider = new TemporaryVariableProvider

  override def visit(expression: Return)(value: Chunk): Chunk = {
    Chunk(value.statements :+ Target.Return(value.expression), RuntimeApi.tuples.unitValue)
  }

  override def visit(expression: VariableDeclaration)(value: Chunk): Chunk = {
    value.flatMap { value =>
      Chunk.unit(Target.VariableDeclaration(expression.variable.targetVariable.name, value, expression.variable.isMutable))
    }
  }

  override def visit(expression: Assignment)(target: Chunk, value: Chunk): Chunk = {
    Chunk.combine(target, value) { case Vector(left, right) => Chunk.unit(left.assign(right)) }
  }

  override def visit(expression: Block)(expressions: Vector[Chunk]): Chunk = Chunk.sequence(expressions)

  override def visit(expression: BindingAccess): Chunk = Chunk.expression(TargetRepresentableTranspiler.transpile(expression.binding))

  override def visit(expression: MemberAccess)(instance: Chunk): Chunk = {
    instance.mapExpression(_.prop(expression.member.name))
  }

  override def visit(literal: Literal): Chunk = {
    val result = literal.tpe match {
      case BasicType.Real => Target.RealLiteral(literal.value.asInstanceOf[Double])
      case BasicType.Int => Target.IntLiteral(literal.value.asInstanceOf[Long])
      case BasicType.Boolean => Target.BooleanLiteral(literal.value.asInstanceOf[Boolean])
      case BasicType.String => Target.StringLiteral(literal.value.asInstanceOf[String])
    }
    Chunk.expression(result)
  }

  override def visit(expression: Tuple)(values: Vector[Chunk]): Chunk = {
    if (expression.tpe == TupleType.UnitType) {
      Chunk.expression(RuntimeApi.tuples.unitValue)
    } else {
      Chunk.combine(values) { values =>
        Chunk.expression(RuntimeApi.tuples.value(values))
      }
    }
  }

  override def visit(expression: AnonymousFunction)(body: Chunk): Chunk = {
    val lambdaParameters = expression.parameters.map(p => Target.Parameter(RuntimeNames.localVariable(p.name).name))
    val lambdaBody = if (body.statements.nonEmpty) {
      Target.Block(body.statements ++ Vector(Target.Return(body.expression)))
    } else body.expression

    Chunk.expression(
      RuntimeApi.functions.value(
        Target.Lambda(lambdaParameters, lambdaBody),
        TypeTranspiler.transpileSubstitute(expression.tpe),
      )
    )
  }

  override def visit(expression: MultiFunctionValue): Chunk = {
    val target = TargetRepresentableTranspiler.transpile(expression.mf)
    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
    Chunk.expression(RuntimeApi.functions.value(target, tpe))
  }

  override def visit(expression: FixedFunctionValue): Chunk = {
    val target = TargetRepresentableTranspiler.transpile(expression.instance.definition)
    val tpe = TypeTranspiler.transpile(expression.tpe)
    Chunk.expression(RuntimeApi.functions.value(target, tpe))
  }

  override def visit(expression: ConstructorValue): Chunk = {
    val schema = expression.structType.schema
    val value = if (schema.isConstant) {
      RuntimeNames.struct.constructor(schema)
    } else {
      val varSchema = RuntimeNames.schema(schema)
      val typeArguments = expression.structType.typeArguments.map(TypeTranspiler.transpile)
      val varConstruct = RuntimeNames.struct.construct(schema)
      RuntimeApi.structs.getConstructor(varSchema, Target.List(typeArguments), varConstruct)
    }
    Chunk.expression(value)
  }

  override def visit(expression: ListConstruction)(values: Vector[Chunk]): Chunk = {
    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
    Chunk.combine(values) { values =>
      Chunk.expression(RuntimeApi.lists.value(values, tpe))
    }
  }

  override def visit(expression: MapConstruction)(entryChunks: Vector[(Chunk, Chunk)]): Chunk = {
    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
    val entries = entryChunks.map { case (key, value) =>
      Chunk.combine(key, value)(elements => Chunk.expression(Target.List(elements)))
    }

    Chunk.combine(entries) { entries =>
      val hash = RuntimeNames.multiFunction(registry.core.hash.name)
      val equal = RuntimeNames.multiFunction(registry.core.equal.name)
      Chunk.expression(RuntimeApi.maps.value(entries, tpe, hash, equal))
    }
  }

  override def visit(expression: ShapeValue)(propertyChunks: Vector[Chunk]): Chunk = {
    // The property types of the shape's type are determined at run-time, so we don't have to transpile the type here.
    Chunk.combine(propertyChunks) { values =>
      val dictionary = Target.Dictionary(
        expression.properties.zip(values).map { case (property, value) =>
          Target.Property(property.name, value)
        }
      )
      Chunk.expression(RuntimeApi.shapes.value(dictionary))
    }
  }

  override def visit(symbol: Symbol): Chunk = Chunk.expression(symbolHistory.targetValue(symbol.name))

  override def visit(expression: UnaryOperation)(value: Chunk): Chunk = {
    val operator = expression.operator match {
      case UnaryOperator.Negation => TargetOperator.Negation
      case UnaryOperator.LogicalNot => TargetOperator.Not
    }
    Chunk.operation(operator, value)
  }

  override def visit(expression: BinaryOperation)(left: Chunk, right: Chunk): Chunk = {
    // Filter those cases first that can't simply be translated to a binary Javascript operator.
    expression.operator match {
      case BinaryOperator.Append => transpileListAppends(left, right, expression.tpe)
      case _ =>
        val operator = expression.operator match {
          case BinaryOperator.Addition => TargetOperator.Addition
          case BinaryOperator.Subtraction => TargetOperator.Subtraction
          case BinaryOperator.Multiplication => TargetOperator.Multiplication
          case BinaryOperator.Division => TargetOperator.Division
          // All the complex cases have been filtered already and we can apply simple comparison.
          case BinaryOperator.Equals => TargetOperator.Equals
          case BinaryOperator.LessThan => TargetOperator.LessThan
          case BinaryOperator.LessThanEquals => TargetOperator.LessThanEquals
        }
        Chunk.operation(operator, left, right)
    }
  }

  private def transpileListAppends(list: Chunk, element: Chunk, resultType: Type): Chunk = {
    val tpe = TypeTranspiler.transpileSubstitute(resultType)
    Chunk.combine(list, element) { case Vector(list, element) =>
      // We might be tempted to use `appendUntyped` here if the element type is already a subtype of the list's element
      // type, but that would be incorrect. Though we can be sure that the given list has AT MOST some type `[t1]` at
      // run time, it might also be typed as a subtype `[t2]` of `[t1]`. If at run time the list is of type `[t2]` and
      // the element has the type `t1`, the append should result in a list of type `[t1]`. appendUntyped would result
      // in a list of type `[t2]`.
      Chunk.expression(RuntimeApi.lists.append(list, element, tpe))
    }
  }

  override def visit(expression: XaryOperation)(operands: Vector[Chunk]): Chunk = {
    val operator = expression.operator match {
      case XaryOperator.Conjunction => TargetOperator.And
      case XaryOperator.Disjunction => TargetOperator.Or
      case XaryOperator.Concatenation => TargetOperator.Concat
    }
    Chunk.operation(operator, operands: _*)
  }

  override def visit(expression: Call)(target: Option[Chunk], arguments: Vector[Chunk]): Chunk = {
    def withArguments(f: Vector[Target.TargetExpression] => Target.TargetExpression) = {
      Chunk.combine(arguments) { arguments => Chunk.expression(f(arguments)) }
    }
    def functionValueCall(function: Target.TargetExpression) = withArguments(RuntimeApi.functions.call(function, _))
    def directCall(expression: Target.TargetExpression) = withArguments(Target.Call(expression, _))

    expression.target match {
      case CallTarget.Value(ConstructorValue(_, structType, _)) =>
        // Optimization: If we're directly calling a constructor value, the function call boils down to calling the
        // `construct` function. This allows us to bypass a run-time call to `getConstructor` for structs with type
        // parameters.
        withArguments(arguments => InstantiationTranspiler.transpileStructInstantiation(structType, arguments))

      case CallTarget.Value(_) => target.get.flatMap(functionValueCall)

      case CallTarget.MultiFunction(mf) => directCall(TargetRepresentableTranspiler.transpile(mf))

      case CallTarget.Constructor(_) =>
        // The result type of the constructor call is the struct with instantiated type parameters. Hence, we can take
        // it here for transpilation.
        val structType = expression.tpe match {
          case structType: StructType => structType
          case _ => throw CompilationException(s"The result type of a constructor must be a struct. Position: ${expression.position}.")
        }
        withArguments(arguments => InstantiationTranspiler.transpileStructInstantiation(structType, arguments))

      case CallTarget.Dynamic(name) => directCall(name.asVariable)
    }
  }

  override def visit(expression: Cond)(cases: Vector[(Chunk, Chunk)]): Chunk = ConditionalTranspiler.transpile(expression, cases)

  override def visit(loop: WhileLoop)(condition: Chunk, body: Chunk): Chunk = LoopTranspiler().transpile(loop, condition, body)

  override def visit(loop: ForLoop)(collections: Vector[Chunk], body: Chunk): Chunk = LoopTranspiler().transpile(loop, collections, body)
}
