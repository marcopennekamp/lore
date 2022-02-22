package lore.compiler.assembly.values

import lore.compiler.assembly.AsmChunk
import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.poem._
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, Literal, UnaryOperator, XaryOperator}
import lore.compiler.types.Type.isPolymorphic
import lore.compiler.utils.CollectionExtensions.{OptionVectorExtension, VectorExtension}

object ValueAssembler {

  /**
    * Generates a PoemValue representation of the given expression IF it can be represented as a constant value. Poem
    * values whose types contain type variables cannot be constant.
    *
    * For now, the algorithm is very simple, only taking literals and some operations into account. We can expand this
    * to be as complex as we want, as far as building an interpreter into the compiler that tries to compute the
    * constant value.
    */
  def generate(expression: Expression): Option[PoemValue] = expression match {
    case Expression.Ascription(value, _, _) => generate(value)

    case Expression.Literal(value, _) =>
      val poemValue = value match {
        case Literal.IntValue(value) => PoemIntValue(value)
        case Literal.RealValue(value) => PoemRealValue(value)
        case Literal.BooleanValue(value) => PoemBooleanValue(value)
        case Literal.StringValue(value) => PoemStringValue(value)
      }
      Some(poemValue)

    case expression@Expression.Tuple(values, _) =>
      // The tuple's type is directly built from its element types, so we can just check the former for type
      // variables.
      if (isPolymorphic(expression.tpe)) {
        return None
      }
      generate(values).map(values => PoemTupleValue(values, TypeAssembler.generate(expression.tpe)))

    case _: Expression.AnonymousFunction =>
      // It is technically possible to turn anonymous functions into constant poem values, but this would complicate
      // the ValueAssembler as it'd have to generate PoemFunctions. If a value cannot be generated due to some other
      // reason, the PoemFunction would be needlessly assembled before being thrown away. In addition, we'd have to
      // carry a FunctionSignature into the ValueAssembler to decide whether the lambda can be a constant value.
      //
      // These two disadvantages are not worth the tradeoff of implementing the constant value semantics for anonymous
      // functions here. Instead, the ExpressionAssembler directly creates such a constant function value if it
      // encounters an AnonymousFunction expression. The disadvantage of this approach is that, for example, a list
      // containing anonymous functions such as `[x => x + 3, x => x * 2]` would not be compiled as a constant value.
      // The major use case, constant lambda values passed as arguments or assigned to a variable, is covered by the
      // semantics of the ExpressionAssembler.
      None

    case Expression.MultiFunctionValue(mf, tpe, _) => Some(PoemMultiFunctionValue(mf.name, TypeAssembler.generate(tpe)))

    case Expression.FixedFunctionValue(instance, _) =>
      val inputType = TypeAssembler.generate(instance.signature.inputType)
      val tpe = TypeAssembler.generate(instance.signature.functionType)
      Some(PoemFixedFunctionValue(instance.definition.multiFunction.name, inputType, tpe))

    // TODO (assembly): Can we turn constructor values into constant poem values?
    case _: Expression.ConstructorValue => None

    case Expression.ListConstruction(values, _) =>
      // In contrast to tuples, a list constant's type isn't guaranteed to contain the types of all its elements. For
      // example, a list `[a, b]` with `a: Any` and `b: X` will have the element type `Any`.
      if (values.exists(v => isPolymorphic(v.tpe))) {
        return None
      }
      generate(values).map(values => PoemListValue(values, TypeAssembler.generate(expression.tpe)))

    // TODO (maps): Implement map poem values.
    case Expression.MapConstruction(_, _) => None

    case Expression.ShapeValue(properties, _) =>
      if (isPolymorphic(expression.tpe)) {
        return None
      }

      properties
        .map(p => generate(p.value).map(p.name -> _)).sequence
        .map(properties => PoemShapeValue(properties.toMap, TypeAssembler.generate(expression.tpe)))

    case Expression.Symbol(name, _) => Some(PoemSymbolValue(name))

    case Expression.UnaryOperation(operator, value, _, _) =>
      generate(value).flatMap { operand =>
        operator match {
          case UnaryOperator.Negation => operand match {
            case PoemIntValue(value) => Some(PoemIntValue(-value))
            case PoemRealValue(value) => Some(PoemRealValue(-value))
            case _ => None
          }

          case UnaryOperator.LogicalNot => operand match {
            case PoemBooleanValue(value) => Some(PoemBooleanValue(!value))
            case _ => None
          }
        }
      }

    case Expression.BinaryOperation(operator, left, right, _, _) =>
      generate(left).flatMap { operand1 =>
        generate(right).flatMap { operand2 =>
          operator match {
            case BinaryOperator.Addition => evaluateArithmeticOperation(operand1, operand2, _ + _, _ + _)
            case BinaryOperator.Subtraction => evaluateArithmeticOperation(operand1, operand2, _ - _, _ - _)
            case BinaryOperator.Multiplication => evaluateArithmeticOperation(operand1, operand2, _ * _, _ * _)
            case BinaryOperator.Division => evaluateArithmeticOperation(operand1, operand2, _ / _, _ / _)

            case BinaryOperator.Equals => Some(PoemBooleanValue(operand1 == operand2))
            case BinaryOperator.LessThan => evaluateOrderOperation(operand1, operand2, _ < _, _ < _)
            case BinaryOperator.LessThanEquals => evaluateOrderOperation(operand1, operand2, _ <= _, _ <= _)

            case BinaryOperator.Append => None
          }
        }
      }

    case Expression.XaryOperation(operator, expressions, _, _) =>
      generate(expressions).flatMap { operands =>
        operator match {
          case XaryOperator.Conjunction =>
            operands.foldSome(true) {
              case (result, PoemBooleanValue(value)) => Some(result && value)
              case (false, _) => Some(false)
              case (true, _) => None
            }.map(PoemBooleanValue)

          case XaryOperator.Disjunction =>
            operands.foldSome(false) {
              case (result, PoemBooleanValue(value)) => Some(result || value)
              case (true, _) => Some(true)
              case (false, _) => None
            }.map(PoemBooleanValue)

          case XaryOperator.Concatenation =>
            // A Concatenation node will only be produced in case of string interpolation, so it's unlikely to lead to
            // a constant string.
            None
        }
      }

    case _ => None
  }

  def generate(expressions: Vector[Expression]): Option[Vector[PoemValue]] = expressions.map(generate).sequence

  /**
    * The forced cousin of [[generate]]. Use this if it's certain that the given expression always results in a
    * PoemValue.
    */
  def generateForced(expression: Expression): PoemValue = generate(expression).get

  /**
    * Generates a chunked `Const` instruction from the given expression if a constant poem value can be generated from
    * it.
    */
  def generateConst(expression: Expression, target: Poem.Register): Option[AsmChunk] = {
    generate(expression).map { constant =>
      val instruction = PoemInstruction.Const(target, constant)
      AsmChunk(target, instruction)
    }
  }

  /**
    * The forced cousin of [[generateConst]]. Use this if it's certain that the given expression always results in a
    * PoemValue.
    */
  def generateConstForced(expression: Expression, target: Poem.Register): AsmChunk = generateConst(expression, target).get

  private def evaluateArithmeticOperation(
    left: PoemValue,
    right: PoemValue,
    intOperator: (Long, Long) => Long,
    realOperator: (Double, Double) => Double,
  ): Option[PoemValue] = {
    val result = (left, right) match {
      case (PoemIntValue(a), PoemIntValue(b)) => PoemIntValue(intOperator(a, b))
      case (PoemIntValue(a), PoemRealValue(b)) => PoemRealValue(realOperator(a.toDouble, b))
      case (PoemRealValue(a), PoemIntValue(b)) => PoemRealValue(realOperator(a, b.toDouble))
      case (PoemRealValue(a), PoemRealValue(b)) => PoemRealValue(realOperator(a, b))
    }
    Some(result)
  }

  private def evaluateOrderOperation(
    left: PoemValue,
    right: PoemValue,
    intOperator: (Long, Long) => Boolean,
    realOperator: (Double, Double) => Boolean,
  ): Option[PoemValue] = {
    val result = (left, right) match {
      case (PoemIntValue(a), PoemIntValue(b)) => intOperator(a, b)
      case (PoemIntValue(a), PoemRealValue(b)) => realOperator(a.toDouble, b)
      case (PoemRealValue(a), PoemIntValue(b)) => realOperator(a, b.toDouble)
      case (PoemRealValue(a), PoemRealValue(b)) => realOperator(a, b)
    }
    Some(PoemBooleanValue(result))
  }

}
