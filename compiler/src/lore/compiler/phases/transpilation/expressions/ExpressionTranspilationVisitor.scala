package lore.compiler.phases.transpilation.expressions

import lore.compiler.phases.transpilation.TypeTranspiler.TranspiledTypeVariables
import lore.compiler.phases.transpilation._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.{CallTarget, FunctionInstance}
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.types._

private[transpilation] class ExpressionTranspilationVisitor()(
  implicit registry: Registry, runtimeTypeVariables: TranspiledTypeVariables
) extends ExpressionVisitor[Chunk, Chunk] {
  import Expression._

  private implicit val variableProvider: TemporaryVariableProvider = new TemporaryVariableProvider

  override def visit(expression: Return)(value: Chunk): Chunk = {
    Chunk(value.statements :+ Target.Return(value.expression), RuntimeApi.tuples.unitValue)
  }

  override def visit(expression: VariableDeclaration)(value: Chunk): Chunk = {
    value.flatMap { value =>
      Chunk.unit(Target.VariableDeclaration(expression.variable.asTargetVariable.name, value, expression.variable.isMutable))
    }
  }

  override def visit(expression: Assignment)(target: Chunk, value: Chunk): Chunk = {
    Chunk.combine(target, value) { case Vector(left, right) => Chunk.unit(left.assign(right)) }
  }

  override def visit(expression: Block)(expressions: Vector[Chunk]): Chunk = Chunk.sequence(expressions)

  override def visit(expression: VariableAccess): Chunk = Chunk.expression(expression.variable.asTargetVariable)

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
    if (expression.tpe == ProductType.UnitType) {
      Chunk.expression(RuntimeApi.tuples.unitValue)
    } else {
      Chunk.combine(values) { values =>
        Chunk.expression(RuntimeApi.tuples.value(values))
      }
    }
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
      Chunk.expression(RuntimeApi.maps.value(entries, tpe, "hash".asVariable, "areEqual".asVariable))
    }
  }

  override def visit(expression: Instantiation)(arguments: Vector[Chunk]): Chunk = {
    Chunk.combine(arguments) { values =>
      // The argument passed to the instantiation function at run-time is an object with the required properties
      // already set. We're building this object here.
      val properties = Target.Dictionary(
        expression.arguments.map(_.property).zip(values).map { case (property, value) =>
          Target.Property(property.name.asName, value)
        }
      )
      Chunk.expression(RuntimeNames.instantiate(expression.struct.tpe).call(properties))
    }
  }

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
    Chunk.combine(list, element) { case Vector(list, element) => Chunk.expression(RuntimeApi.lists.append(list, element, tpe)) }
  }

  override def visit(expression: XaryOperation)(operands: Vector[Chunk]): Chunk = {
    val operator = expression.operator match {
      case XaryOperator.Conjunction => TargetOperator.And
      case XaryOperator.Disjunction => TargetOperator.Or
      case XaryOperator.Concatenation => TargetOperator.Concat
    }
    Chunk.operation(operator, operands: _*)
  }

  override def visit(expression: Call)(arguments: Vector[Chunk]): Chunk = {
    expression.target match {
      case _: FunctionInstance | _: CallTarget.Dynamic =>
        Chunk.combine(arguments) { arguments => Chunk.expression(Target.Call(expression.target.name.asVariable, arguments)) }
    }
  }

  override def visit(expression: IfElse)(condition: Chunk, onTrue: Chunk, onFalse: Chunk): Chunk = {
    // If the result type of the if-else is unit, we can ignore the results of the respective then and else blocks.
    val varResult = if (expression.tpe != ProductType.UnitType) Some(variableProvider.createVariable()) else None
    val resultDeclaration = varResult.map(_.declareMutableAs(Target.Undefined))

    def asBlock(chunk: Chunk): Target.Block = {
      // If there is a result variable, we want to assign the chunk's expression to the variable. Otherwise, we still
      // want the expression to execute to preserve potential side effects! (But only if it's not a unit value.)
      val lastStatement = varResult.map(_.assign(chunk.expression)).orElse(chunk.meaningfulExpression)
      Target.Block(chunk.statements ++ lastStatement.toVector)
    }

    val ifElse = Target.IfElse(condition.expression, asBlock(onTrue), asBlock(onFalse))

    Chunk(
      condition.statements ++ resultDeclaration.toVector ++ Vector(ifElse),
      varResult.getOrElse(RuntimeApi.tuples.unitValue)
    )
  }

  override def visit(loop: WhileLoop)(condition: Chunk, body: Chunk): Chunk = LoopTranspiler().transpile(loop, condition, body)

  override def visit(loop: ForLoop)(collections: Vector[Chunk], body: Chunk): Chunk = LoopTranspiler().transpile(loop, collections, body)
}