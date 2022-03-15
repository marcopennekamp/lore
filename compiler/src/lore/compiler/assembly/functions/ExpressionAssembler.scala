package lore.compiler.assembly.functions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.values.ValueAssembler
import lore.compiler.assembly.{AsmChunk, PropertyOrder, RegisterProvider}
import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.PoemInstruction.InstanceKind
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.ParameterDefinition.NamedParameterView
import lore.compiler.semantics.functions.{CallTarget, FunctionSignature}
import lore.compiler.semantics.scopes.LocalVariable
import lore.compiler.types._

import scala.collection.immutable.HashMap

// TODO (assembly): There is a difference between an expression returning Unit and an expression's result not being
//                  used. In the case of a loop, Unit incidentally also expresses that the result is not being used,
//                  but only due to a "hack" inside Expression.Loop. In other cases, e.g. for Cond, without an "unused
//                  expression" analysis, we have to assume that the Cond should always result in a target, so we at
//                  least have to assign Unit to a target register if its result type is Unit. So, introducing a
//                  separate "unused expression" analysis would improve our ability to generate more optimal code
//                  without resorting to result type hacks.

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
      throw CompilationException(s"The variable ${variable.name} at $position is already declared somewhere else.")
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

  def generate(expression: Expression): AsmChunk = {
    expression match {
      case _: Hole => throw CompilationException("Expression.Hole cannot be assembled.")
      case node: Return => handle(node)
      case node: VariableDeclaration => handle(node)
      case node: Assignment => handle(node)
      case node: Block => handle(node)
      case node: BindingAccess => handle(node)
      case node: MemberAccess => handle(node)
      case _: UnresolvedMemberAccess => throw CompilationException("Expression.UnresolvedMemberAccess cannot be assembled.")
      case node: Literal => handle(node)
      case node: Tuple => handle(node)
      case node: AnonymousFunction => handle(node)
      case node: MultiFunctionValue => handle(node)
      case node: FixedFunctionValue => handle(node)
      case node: ConstructorValue => handle(node)
      case _: UntypedConstructorValue => throw CompilationException("Expression.UntypedConstructorValue cannot be assembled.")
      case node: ListConstruction => handle(node)
      case node: MapConstruction => handle(node)
      case node: ShapeValue => handle(node)
      case node: Symbol => handle(node)
      case node: PropertyDefaultValue => handle(node)
      case node: UnaryOperation => handle(node)
      case node: BinaryOperation => handle(node)
      case node: XaryOperation => handle(node)
      case node: Call => handle(node)
      case node: Cond => handle(node)
      case node: WhileLoop => handle(node)
      case node: ForLoop => handle(node)
      case node: Ascription => handle(node)
    }
  }

  def generate(expressions: Vector[Expression]): Vector[AsmChunk] = expressions.map(generate)

  private def handle(expression: Return): AsmChunk = {
    val valueChunk = generate(expression.value)
    val instruction = PoemInstruction.Return(valueChunk.forceResult(expression.position))
    valueChunk ++ AsmChunk(instruction)
  }

  private def handle(expression: VariableDeclaration): AsmChunk = {
    val valueChunk = generate(expression.value)
    val regVariable = declare(expression.variable, expression.position)
    val regValue = valueChunk.forceResult(expression.position)
    val assignment = PoemInstruction.Assign(regVariable, regValue)
    valueChunk ++ AsmChunk(assignment)
  }

  private def handle(expression: Assignment): AsmChunk = {
    val valueChunk = generate(expression.value)
    val regValue = valueChunk.forceResult(expression.value.position)
    expression.target match {
      case Expression.BindingAccess(binding, position) =>
        val regVariable = binding match {
          case variable: LocalVariable => variableRegisterMap(variable.uniqueKey)
          case _ => throw CompilationException(s"Binding $binding cannot be mutable. Position: $position.")
        }
        valueChunk ++ AsmChunk(PoemInstruction.Assign(regVariable, regValue))

      case Expression.MemberAccess(instance, member, position) =>
        if (!instance.tpe.isInstanceOf[StructType]) {
          throw CompilationException(s"Only struct members may be mutated directly, but the instance's type is ${instance.tpe}. Position: $position.")
        }

        val instanceKind = InstanceKind.of(instance.tpe)
        val instanceChunk = generate(instance)
        val regInstance = instanceChunk.forceResult(position)
        valueChunk ++ instanceChunk ++ AsmChunk(
          PoemInstruction.PropertySet(instanceKind, regInstance, member.name, regValue),
        )

      case _ => throw CompilationException(s"Invalid assignment target. Position: ${expression.target.position}.")
    }
  }

  private def handle(expression: Block): AsmChunk = AsmChunk.concat(generate(expression.expressions))

  private def handle(expression: BindingAccess): AsmChunk = TypedBindingAssembler.generate(expression.binding)

  private def handle(expression: MemberAccess): AsmChunk = {
    val target = registerProvider.fresh()
    val instanceKind = InstanceKind.of(expression.instance.tpe)
    val instanceChunk = generate(expression.instance)
    val regInstance = instanceChunk.forceResult(expression.position)
    val instruction = PoemInstruction.PropertyGet(target, instanceKind, regInstance, expression.member.name)
    instanceChunk ++ AsmChunk(target, instruction)
  }

  private def handle(literal: Literal): AsmChunk = {
    val regResult = registerProvider.fresh()
    ValueAssembler.generateConstForced(literal, regResult)
  }

  private def handle(expression: Tuple): AsmChunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val valueChunks = generate(expression.values)
      val instruction = PoemInstruction.Tuple(target, valueChunks.map(_.forceResult(expression.position)))
      AsmChunk.concat(valueChunks) ++ AsmChunk(target, instruction)
    }
  }

  private def handle(expression: AnonymousFunction): AsmChunk = {
    val (chunk, poemFunctions) = LambdaAssembler.generate(expression, signature)
    generatedPoemFunctions ++= poemFunctions
    chunk
  }

  private def handle(expression: MultiFunctionValue): AsmChunk = {
    ValueAssembler.generateConstForced(expression, registerProvider.fresh())
  }

  private def handle(expression: FixedFunctionValue): AsmChunk = {
    ValueAssembler.generateConstForced(expression, registerProvider.fresh())
  }

  private def handle(expression: ConstructorValue): AsmChunk = {
    val regResult = registerProvider.fresh()
    ValueAssembler.generateConst(expression, regResult).getOrElse {
      ???
    }
  }

  private def handle(expression: ListConstruction): AsmChunk = {
    val regResult = registerProvider.fresh()
    ValueAssembler.generateConst(expression, regResult).getOrElse {
      val valueChunks = generate(expression.values)
      val tpe = TypeAssembler.generate(expression.tpe)
      val instruction = PoemInstruction.List(regResult, tpe, valueChunks.map(_.forceResult(expression.position)))
      AsmChunk.concat(valueChunks) ++ AsmChunk(regResult, instruction)
    }
  }

  private def handle(expression: MapConstruction): AsmChunk = {
    val entryChunks = expression.entries.map(entry => (generate(entry.key), generate(entry.value)))
//    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
//    val entries = entryChunks.map { case (key, value) =>
//      AsmChunk.combine(key, value)(elements => AsmChunk.expression(Target.List(elements)))
//    }
//
//    AsmChunk.combine(entries) { entries =>
//      val hash = RuntimeNames.multiFunction(registry.core.hash.name)
//      val equal = RuntimeNames.multiFunction(registry.core.equal.name)
//      AsmChunk.expression(RuntimeApi.maps.value(entries, tpe, hash, equal))
//    }
    ???
  }

  private def handle(expression: ShapeValue): AsmChunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val propertyChunks = expression.properties.map(property => property.name -> generate(property.value))
      val (sortedNames, sortedChunks) = PropertyOrder.sort(propertyChunks)(_._1).unzip
      val metaShape = PoemMetaShape.build(sortedNames)
      val propertyRegisters = sortedChunks.map(_.forceResult(expression.position))
      val instruction = PoemInstruction.Shape(target, metaShape, propertyRegisters)
      AsmChunk.concat(sortedChunks) ++ AsmChunk(target, instruction)
    }
  }

  private def handle(symbol: Symbol): AsmChunk = {
    ValueAssembler.generateConstForced(symbol, registerProvider.fresh())
  }

  private def handle(expression: PropertyDefaultValue): AsmChunk = {
    ConstructorCallAssembler.generateDefaultPropertyCall(expression.property)
  }

  private def handle(expression: UnaryOperation): AsmChunk = {
    val valueChunk = generate(expression.value)
    PrimitiveOperationAssembler.generateUnaryOperation(expression, valueChunk)
  }

  private def handle(expression: BinaryOperation): AsmChunk = {
    val leftChunk = generate(expression.left)
    val rightChunk = generate(expression.right)

    // Operators which cannot be translated as primitives are filtered first. All the non-primitive cases for Equals,
    // LessThan, and LessThanEquals have been filtered already.
    expression.operator match {
      case BinaryOperator.Append => transpileListAppends(expression, leftChunk, rightChunk)
      case _ => PrimitiveOperationAssembler.generateBinaryOperation(expression, leftChunk, rightChunk)
    }
  }

  private def transpileListAppends(expression: BinaryOperation, listChunk: AsmChunk, elementChunk: AsmChunk): AsmChunk = {
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
    listChunk ++ elementChunk ++ AsmChunk(target, instruction)
  }

  private def handle(expression: XaryOperation): AsmChunk = {
    val operandChunks = generate(expression.expressions)
    PrimitiveOperationAssembler.generateXaryOperation(expression, operandChunks)
  }

  private def handle(expression: Call): AsmChunk = {
    val argumentChunks = generate(expression.arguments)

    val regResult = registerProvider.fresh()
    val valueArgumentRegs = argumentChunks.map(_.forceResult(expression.position))

    // TODO (assembly): Handle direct constructor value calls.
    val callChunk = expression.target match {
      case CallTarget.MultiFunction(mf) => AsmChunk(regResult, PoemInstruction.Dispatch(regResult, mf.name, valueArgumentRegs))

      case CallTarget.Value(ConstructorValue(_, structType, _)) =>
        // Optimization: We can treat a direct constructor value call as a constructor call.
        ConstructorCallAssembler.generate(structType, regResult, valueArgumentRegs)

      case CallTarget.Value(function) =>
        val functionChunk = generate(function)
        val regFunction = functionChunk.forceResult(function.position)
        functionChunk ++ AsmChunk(regResult, PoemInstruction.FunctionCall(regResult, regFunction, valueArgumentRegs))

      case CallTarget.Constructor(_) =>
        val structType = expression.tpe.asInstanceOf[StructType]
        ConstructorCallAssembler.generate(structType, regResult, valueArgumentRegs)

      case CallTarget.Dynamic(intrinsic) =>
        // TODO (assembly): If the Call has been analyzed to be unused, we can use `IntrinsicVoid`.
        AsmChunk(regResult, PoemInstruction.Intrinsic(regResult, intrinsic, valueArgumentRegs))
    }

    AsmChunk.concat(argumentChunks) ++ callChunk
  }

  private def handle(cond: Cond): AsmChunk = {
    val caseChunks = cond.cases.map(condCase => (generate(condCase.condition), generate(condCase.body)))
    CondAssembler.generate(cond, caseChunks)
  }

  private def handle(loop: WhileLoop): AsmChunk = {
    val conditionChunk = generate(loop.condition)
    val bodyChunk = generate(loop.body)
    LoopAssembler.generate(loop, conditionChunk, bodyChunk)
  }

  private def handle(loop: ForLoop): AsmChunk = {
    // We have to assign registers to all extractor element variables so that the loop's body can access them.
    loop.extractors.foreach { extractor =>
      variableRegisterMap += (extractor.variable.uniqueKey -> registerProvider.fresh())
    }

    val collectionChunks = generate(loop.extractors.map(_.collection))
    val bodyChunk = generate(loop.body)
    LoopAssembler.generate(loop, collectionChunks, bodyChunk)
  }

  private def handle(expression: Ascription): AsmChunk = generate(expression.value)
}
