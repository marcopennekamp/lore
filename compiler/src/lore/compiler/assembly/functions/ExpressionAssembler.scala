package lore.compiler.assembly.functions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.assembly.{Chunk, PropertyOrder, RegisterProvider}
import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.PoemInstruction.InstanceKind
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.functions.ParameterDefinition.NamedParameterView
import lore.compiler.semantics.functions.{CallTarget, FunctionSignature}
import lore.compiler.types._

import scala.collection.immutable.HashMap

/**
  * The expression assembler is not an ExpressionVisitor because select expressions either don't need their child
  * chunks in certain situations (e.g. when a list can be turned into a [[PoemListValue]]), or generating a child chunk
  * is even nonsensical (in the case of anonymous functions).
  *
  * To support anonymous function expressions, the assembler keeps a list of additionally generated functions, which
  * must be included in the [[FunctionAssembler]]'s result.
  *
  * The assembler generates Jump instructions with <i>label</i> locations. They will later be converted to absolute
  * locations when instructions are flattened.
  */
class ExpressionAssembler(
  signature: FunctionSignature,
  capturedVariables: CapturedVariableMap,
)(implicit registry: Registry) {
  import Expression._

  var generatedPoemFunctions: Vector[PoemFunction] = Vector.empty

  private implicit val registerProvider: RegisterProvider = new RegisterProvider
  private implicit var variableRegisterMap: VariableRegisterMap = HashMap.empty
  private implicit val capturedVariableMap: CapturedVariableMap = capturedVariables

  private def declare(variable: LocalVariable, position: Position): Poem.Register = {
    if (variableRegisterMap.contains(variable.uniqueKey)) {
      throw CompilationException(s"The variable `${variable.name}` at $position is already declared somewhere else.")
    }

    val register = registerProvider.fresh()
    variableRegisterMap += (variable.uniqueKey -> register)
    register
  }

  // The first N registers of the function are reserved for the parameters.
  signature.parameters.foreach { parameter =>
    parameter.name match {
      case Some(_) => declare(NamedParameterView(parameter).asVariable, parameter.position)
      case None =>
        // This register won't be used, but calling `fresh` is still important so that the register IDs are counted up,
        // which have to match for subsequent parameters.
        registerProvider.fresh()
    }
  }

  def generate(expression: Expression): Chunk = {
    expression match {
      case _: Hole => throw CompilationException("Expression.Hole cannot be assembled.")
      case expression: Return => handle(expression)
      case expression: VariableDeclaration => handle(expression)
      case expression: Assignment => handle(expression)
      case expression: Block => handle(expression)
      case expression: BindingAccess => handle(expression)
      case expression: MemberAccess => handle(expression)
      case _: UnresolvedMemberAccess => throw CompilationException("Expression.UnresolvedMemberAccess cannot be assembled.")
      case expression: Literal => handle(expression)
      case expression: TupleValue => handle(expression)
      case expression: LambdaValue => handle(expression)
      case expression: MultiFunctionValue => handle(expression)
      case expression: FixedFunctionValue => handle(expression)
      case expression: ConstructorValue => handle(expression)
      case _: UntypedConstructorValue => throw CompilationException("Expression.UntypedConstructorValue cannot be assembled.")
      case expression: ListValue => handle(expression)
      case expression: MapConstruction => handle(expression)
      case expression: ShapeValue => handle(expression)
      case expression: PropertyDefaultValue => handle(expression)
      case expression: UnaryOperation => handle(expression)
      case expression: BinaryOperation => handle(expression)
      case expression: XaryOperation => handle(expression)
      case expression: Call => handle(expression)
      case expression: Cond => handle(expression)
      case expression: WhileLoop => handle(expression)
      case expression: ForLoop => handle(expression)
      case expression: Ascription => handle(expression)
    }
  }

  def generate(expressions: Vector[Expression]): Vector[Chunk] = expressions.map(generate)

  private def handle(expression: Return): Chunk = {
    val valueChunk = generate(expression.value)
    val instruction = PoemInstruction.Return(valueChunk.forceResult(expression.position))
    valueChunk ++ Chunk(instruction)
  }

  private def handle(expression: VariableDeclaration): Chunk = {
    val valueChunk = generate(expression.value)
    val regVariable = declare(expression.variable, expression.position)
    val regValue = valueChunk.forceResult(expression.position)
    val assignment = PoemInstruction.Assign(regVariable, regValue)
    valueChunk ++ Chunk(assignment)
  }

  private def handle(expression: Assignment): Chunk = {
    val valueChunk = generate(expression.value)
    val regValue = valueChunk.forceResult(expression.value.position)
    expression.target match {
      case Expression.BindingAccess(binding, position) =>
        val regVariable = binding match {
          case variable: LocalVariable => variableRegisterMap(variable.uniqueKey)
          case _ => throw CompilationException(s"Binding $binding cannot be mutable. Position: $position.")
        }
        valueChunk ++ Chunk(PoemInstruction.Assign(regVariable, regValue))

      case Expression.MemberAccess(instance, member, position) =>
        if (!instance.tpe.isInstanceOf[StructType]) {
          throw CompilationException(s"Only struct members may be mutated directly, but the instance's type is ${instance.tpe}. Position: $position.")
        }

        val instanceKind = InstanceKind.of(instance.tpe)
        val instanceChunk = generate(instance)
        val regInstance = instanceChunk.forceResult(position)
        valueChunk ++ instanceChunk ++ Chunk(
          PoemInstruction.PropertySet(instanceKind, regInstance, member.name, regValue),
        )

      case _ => throw CompilationException(s"Invalid assignment target. Position: ${expression.target.position}.")
    }
  }

  private def handle(expression: Block): Chunk = Chunk.concat(generate(expression.expressions))

  private def handle(expression: BindingAccess): Chunk = TypedTermBindingAssembler.generate(expression.binding)

  private def handle(expression: MemberAccess): Chunk = {
    val target = registerProvider.fresh()
    val instanceKind = InstanceKind.of(expression.instance.tpe)
    val instanceChunk = generate(expression.instance)
    val regInstance = instanceChunk.forceResult(expression.position)
    val instruction = PoemInstruction.PropertyGet(target, instanceKind, regInstance, expression.member.name)
    instanceChunk ++ Chunk(target, instruction)
  }

  private def handle(literal: Literal): Chunk = {
    ValueAssembler.generateConstForced(literal, registerProvider.fresh())
  }

  private def handle(expression: TupleValue): Chunk = {
    // A unit value that is unused can be safely ignored. This is an important optimization for unused `if` expressions
    // whose `else` part is unspecified and thus `()`.
    if (expression.isUnused && expression.elements.isEmpty) {
      return Chunk.empty
    }

    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val valueChunks = generate(expression.elements)
      val instruction = PoemInstruction.Tuple(target, valueChunks.map(_.forceResult(expression.position)))
      Chunk.concat(valueChunks) ++ Chunk(target, instruction)
    }
  }

  private def handle(expression: LambdaValue): Chunk = {
    val (chunk, poemFunctions) = LambdaAssembler.generate(expression, signature)
    generatedPoemFunctions ++= poemFunctions
    chunk
  }

  private def handle(expression: MultiFunctionValue): Chunk = {
    ValueAssembler.generateConstForced(expression, registerProvider.fresh())
  }

  private def handle(expression: FixedFunctionValue): Chunk = {
    ValueAssembler.generateConstForced(expression, registerProvider.fresh())
  }

  private def handle(expression: ConstructorValue): Chunk = ConstructorAssembler.generateValue(expression)

  private def handle(expression: ListValue): Chunk = {
    val regResult = registerProvider.fresh()
    ValueAssembler.generateConst(expression, regResult).getOrElse {
      val valueChunks = generate(expression.elements)
      val tpe = TypeAssembler.generate(expression.tpe)
      val instruction = PoemInstruction.List(regResult, tpe, valueChunks.map(_.forceResult(expression.position)))
      Chunk.concat(valueChunks) ++ Chunk(regResult, instruction)
    }
  }

  private def handle(expression: MapConstruction): Chunk = {
    val entryChunks = expression.entries.map(entry => (generate(entry.key), generate(entry.value)))
//    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
//    val entries = entryChunks.map { case (key, value) =>
//      Chunk.combine(key, value)(elements => Chunk.expression(Target.List(elements)))
//    }
//
//    Chunk.combine(entries) { entries =>
//      val hash = RuntimeNames.multiFunction(registry.core.hash.name)
//      val equal = RuntimeNames.multiFunction(registry.core.equal.name)
//      Chunk.expression(RuntimeApi.maps.value(entries, tpe, hash, equal))
//    }
    ???
  }

  private def handle(expression: ShapeValue): Chunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val propertyChunks = expression.properties.map(property => property.name -> generate(property.value))
      val (sortedNames, sortedChunks) = PropertyOrder.sort(propertyChunks)(_._1).unzip
      val metaShape = PoemMetaShape.build(sortedNames)
      val propertyRegisters = sortedChunks.map(_.forceResult(expression.position))
      val instruction = PoemInstruction.Shape(target, metaShape, propertyRegisters)
      Chunk.concat(sortedChunks) ++ Chunk(target, instruction)
    }
  }

  private def handle(expression: PropertyDefaultValue): Chunk = {
    ConstructorAssembler.generatePropertyDefault(expression.property)
  }

  private def handle(expression: UnaryOperation): Chunk = {
    val valueChunk = generate(expression.value)
    PrimitiveOperationAssembler.generateUnaryOperation(expression, valueChunk)
  }

  private def handle(expression: BinaryOperation): Chunk = {
    val leftChunk = generate(expression.left)
    val rightChunk = generate(expression.right)

    // Operators which cannot be translated as primitives are filtered first. All the non-primitive cases for Equals,
    // LessThan, and LessThanEquals have been filtered already.
    expression.operator match {
      case BinaryOperator.Append => transpileListAppends(expression, leftChunk, rightChunk)
      case _ => PrimitiveOperationAssembler.generateBinaryOperation(expression, leftChunk, rightChunk)
    }
  }

  private def transpileListAppends(expression: BinaryOperation, listChunk: Chunk, elementChunk: Chunk): Chunk = {
    // We might be tempted to use `ListAppendUntyped` if `expression.element.tpe` is a subtype of
    // `expression.list.tpe`, but that would be incorrect. Though we can be sure that the given list has at most some
    // type `[t1]` at run time, it might also be typed as a subtype `[t2]` of `[t1]`. If at run time the list is of
    // type `[t2]` and the element has the type `t1`, the append should result in a list of type `[t1]`.
    // `ListAppendUntyped` would result in a list of type `[t2]`.
    val target = registerProvider.fresh()
    val list = listChunk.forceResult(expression.position)
    val element = elementChunk.forceResult(expression.position)
    val tpe = TypeAssembler.generate(expression.tpe)
    val instruction = PoemInstruction.ListAppend(PoemOperation.ListAppend, target, list, element, tpe)
    listChunk ++ elementChunk ++ Chunk(target, instruction)
  }

  private def handle(expression: XaryOperation): Chunk = {
    val operandChunks = generate(expression.operands)
    PrimitiveOperationAssembler.generateXaryOperation(expression, operandChunks)
  }

  private def handle(expression: Call): Chunk = {
    val argumentChunks = generate(expression.arguments)

    val regResult = registerProvider.fresh()
    val valueArgumentRegs = argumentChunks.map(_.forceResult(expression.position))

    val callChunk = expression.target match {
      case CallTarget.MultiFunction(mf) => Chunk(regResult, PoemInstruction.Dispatch(regResult, mf.name, valueArgumentRegs))

      case CallTarget.Value(ConstructorValue(_, structType, _)) =>
        // Optimization: We can treat a direct constructor value call as a constructor call.
        ConstructorAssembler.generateCall(structType, regResult, valueArgumentRegs)

      case CallTarget.Value(function) =>
        val functionChunk = generate(function)
        val regFunction = functionChunk.forceResult(function.position)
        functionChunk ++ Chunk(regResult, PoemInstruction.FunctionCall(regResult, regFunction, valueArgumentRegs))

      case CallTarget.Constructor(_) =>
        val structType = expression.tpe.asInstanceOf[StructType]
        ConstructorAssembler.generateCall(structType, regResult, valueArgumentRegs)

      case CallTarget.Intrinsic(intrinsic) => IntrinsicAssembler.generate(expression, intrinsic, regResult, valueArgumentRegs)
    }

    Chunk.concat(argumentChunks) ++ callChunk
  }

  private def handle(cond: Cond): Chunk = {
    val caseChunks = cond.cases.map(condCase => (generate(condCase.condition), generate(condCase.body)))
    CondAssembler.generate(cond, caseChunks)
  }

  private def handle(loop: WhileLoop): Chunk = {
    val conditionChunk = generate(loop.condition)
    val bodyChunk = generate(loop.body)
    LoopAssembler.generate(loop, conditionChunk, bodyChunk)
  }

  private def handle(loop: ForLoop): Chunk = {
    // We have to assign registers to all extractor element variables so that the loop's body can access them.
    loop.extractors.foreach { extractor =>
      variableRegisterMap += (extractor.variable.uniqueKey -> registerProvider.fresh())
    }

    val collectionChunks = generate(loop.extractors.map(_.collection))
    val bodyChunk = generate(loop.body)
    LoopAssembler.generate(loop, collectionChunks, bodyChunk)
  }

  private def handle(expression: Ascription): Chunk = generate(expression.value)
}
