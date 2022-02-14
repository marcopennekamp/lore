package lore.compiler.assembly.expressions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.{AsmChunk, PropertyOrder, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem.PoemInstruction.PropertyGetInstanceKind
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.semantics.functions.CallTarget
import lore.compiler.types._

import scala.collection.immutable.HashMap

// TODO (assembly): Remember to insert implicit conversions from Int to Real values for arithmetic and comparison expressions.

// TODO (assembly): There is a difference between an expression returning Unit and an expression's result not being
//                  used. In the case of a loop, Unit incidentally also expresses that the result is not being used,
//                  but only due to a "hack" inside Expression.Loop. In other cases, e.g. for Cond, without an "unused
//                  expression" analysis, we have to assume that the Cond should always result in a target, so we at
//                  least have to assign Unit to a target register if its result type is Unit. So, introducing a
//                  separate "unused expression" analysis would improve our ability to generate more optimal code
//                  without resorting to result type hacks.

/**
  * The visitor should generate Jump instructions with <i>label</i> locations. They will later be converted to
  * absolute locations when instructions are flattened.
  */
class ExpressionAssemblyVisitor()(implicit registry: Registry) extends ExpressionVisitor[AsmChunk, AsmChunk] {
  import Expression._

  // TODO (assembly): We have to make sure that the first N registers of the function are reserved for the parameters.
  //                  This also has to be taken into account by the register allocator, who may not reassign these
  //                  registers.
  private implicit val registerProvider: RegisterProvider = new RegisterProvider
  private implicit var variableRegisterMap: VariableRegisterMap = HashMap.empty

  override def visit(expression: Return)(valueChunk: AsmChunk): AsmChunk = {
    val instruction = PoemInstruction.Return(valueChunk.forceResult(expression.position))
    valueChunk ++ AsmChunk(instruction)
  }

  override def visit(expression: VariableDeclaration)(valueChunk: AsmChunk): AsmChunk = {
    val variable = expression.variable
    if (variableRegisterMap.contains(variable.uniqueKey)) {
      throw CompilationException(s"The variable ${variable.name} at ${expression.position} is already declared somewhere else.")
    }

    val variableRegister = registerProvider.fresh()
    variableRegisterMap += (variable.uniqueKey -> variableRegister)

    val valueRegister = valueChunk.forceResult(expression.position)
    val assignment = PoemInstruction.Assign(variableRegister, valueRegister)
    valueChunk ++ AsmChunk(assignment)
  }

  override def visit(expression: Assignment)(targetChunk: AsmChunk, valueChunk: AsmChunk): AsmChunk = {
    val instruction = PoemInstruction.Assign(
      targetChunk.forceResult(expression.position),
      valueChunk.forceResult(expression.position)
    )
    targetChunk ++ valueChunk ++ AsmChunk(instruction)
  }

  override def visit(expression: Block)(expressionChunks: Vector[AsmChunk]): AsmChunk = AsmChunk.concat(expressionChunks)

  override def visit(expression: BindingAccess): AsmChunk = TargetRepresentableAssembler.generate(expression.binding)

  override def visit(expression: MemberAccess)(instanceChunk: AsmChunk): AsmChunk = {
    // TODO (assembly): This doesn't cover all types optimally. For example, an intersection or sum type of two traits
    //                  could be classified as `.Trait`, but is currently classified as `.Any`.
    val instanceKind = expression.tpe match {
      case _: ShapeType => PropertyGetInstanceKind.Shape
      case _: TraitType => PropertyGetInstanceKind.Trait
      case tpe: StructType => PropertyGetInstanceKind.Struct(tpe.schema)
      case _ => PropertyGetInstanceKind.Any
    }
    val target = registerProvider.fresh()
    val instance = instanceChunk.forceResult(expression.position)
    val instruction = PoemInstruction.PropertyGet(target, instanceKind, instance, expression.member.name)
    instanceChunk ++ AsmChunk(target, instruction)
  }

  override def visit(literal: Literal): AsmChunk = {
    val target = registerProvider.fresh()
    val instruction = literal.value match {
      case Literal.IntValue(value) =>
        if (Poem.minDirectInteger <= value && value <= Poem.maxDirectInteger) {
          PoemInstruction.IntConst(target, value.toInt)
        } else {
          PoemInstruction.Const(target, PoemIntValue(value))
        }
      case Literal.RealValue(value) => PoemInstruction.Const(target, PoemRealValue(value))
      case Literal.BooleanValue(value) => PoemInstruction.BooleanConst(target, value)
      case Literal.StringValue(value) => PoemInstruction.Const(target, PoemStringValue(value))
    }
    AsmChunk(target, instruction)
  }

  // TODO (assembly): Perhaps converting Const(_, PoemIntValue(x)) to IntConst(x) should be a separate optimization
  //                  step, because PoemValueAssembler.generateConst will otherwise generate instructions such as
  //                  Const(_, PoemIntValue(x)). Bonus points: We can call this "const smashing".
  // TODO (assembly): If PoemValueAssembler can generate the Const instruction here, `values` has been built completely
  //                  needlessly. If this happens in a lot of cases, we might have to move from a visitor approach to
  //                  pattern matching to better steer control flow.

  override def visit(expression: Tuple)(values: Vector[AsmChunk]): AsmChunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val instruction = PoemInstruction.Tuple(target, values.map(_.forceResult(expression.position)))
      AsmChunk.concat(values) ++ AsmChunk(target, instruction)
    }
  }

  override def visit(expression: AnonymousFunction)(body: AsmChunk): AsmChunk = {
    // TODO (assembly): This is gonna be complex, man.

    /*
    val lambdaParameters = expression.parameters.map(p => Target.Parameter(RuntimeNames.localVariable(p.name).name))
    val lambdaBody = if (body.statements.nonEmpty) {
      Target.Block(body.statements ++ Vector(Target.Return(body.expression)))
    } else body.expression

    AsmChunk.expression(
      RuntimeApi.functions.value(
        Target.Lambda(lambdaParameters, lambdaBody),
        TypeTranspiler.transpileSubstitute(expression.tpe),
      )
    )
    */
    ???
  }

  override def visit(expression: MultiFunctionValue): AsmChunk = {
//    val target = TargetRepresentableTranspiler.transpile(expression.mf)
//    val tpe = TypeTranspiler.transpileSubstitute(expression.tpe)
//    AsmChunk.expression(RuntimeApi.functions.value(target, tpe))
    ???
  }

  override def visit(expression: FixedFunctionValue): AsmChunk = {
//    val target = TargetRepresentableTranspiler.transpile(expression.instance.definition)
//    val tpe = TypeTranspiler.transpile(expression.tpe)
//    AsmChunk.expression(RuntimeApi.functions.value(target, tpe))
    ???
  }

  override def visit(expression: ConstructorValue): AsmChunk = {
//    val schema = expression.structType.schema
//    val value = if (schema.isConstant) {
//      RuntimeNames.struct.constructor(schema)
//    } else {
//      val varSchema = RuntimeNames.schema(schema)
//      val typeArguments = expression.structType.typeArguments.map(TypeTranspiler.transpile)
//      val varConstruct = RuntimeNames.struct.construct(schema)
//      RuntimeApi.structs.getConstructor(varSchema, Target.List(typeArguments), varConstruct)
//    }
//    AsmChunk.expression(value)
    ???
  }

  override def visit(expression: ListConstruction)(values: Vector[AsmChunk]): AsmChunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val tpe = TypeAssembler.generate(expression.tpe)
      val instruction = PoemInstruction.List(target, tpe, values.map(_.forceResult(expression.position)))
      AsmChunk.concat(values) ++ AsmChunk(target, instruction)
    }
  }

  override def visit(expression: MapConstruction)(entryChunks: Vector[(AsmChunk, AsmChunk)]): AsmChunk = {
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

  override def visit(expression: ShapeValue)(propertyChunks: Vector[AsmChunk]): AsmChunk = {
    val target = registerProvider.fresh()
    ValueAssembler.generateConst(expression, target).getOrElse {
      val (sortedNames, sortedChunks) = PropertyOrder.sort(expression.properties.map(_.name).zip(propertyChunks))(_._1).unzip
      val metaShape = PoemMetaShape.build(sortedNames)
      val propertyRegisters = sortedChunks.map(_.forceResult(expression.position))
      val instruction = PoemInstruction.Shape(target, metaShape, propertyRegisters)
      AsmChunk.concat(sortedChunks) ++ AsmChunk(target, instruction)
    }
  }

  override def visit(symbol: Symbol): AsmChunk = {
    val target = registerProvider.fresh()
    val instruction = PoemInstruction.Const(target, PoemSymbolValue(symbol.name))
    AsmChunk(target, instruction)
  }

  override def visit(expression: UnaryOperation)(valueChunk: AsmChunk): AsmChunk = {
    PrimitiveOperationAssembler.generateUnaryOperation(expression, valueChunk)
  }

  override def visit(expression: BinaryOperation)(leftChunk: AsmChunk, rightChunk: AsmChunk): AsmChunk = {
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

  override def visit(expression: XaryOperation)(operands: Vector[AsmChunk]): AsmChunk = {
    PrimitiveOperationAssembler.generateXaryOperation(expression, operands)
  }

  override def visit(expression: Call)(target: Option[AsmChunk], arguments: Vector[AsmChunk]): AsmChunk = {
    val regResult = registerProvider.fresh()
    val argumentRegs = arguments.map(_.forceResult(expression.position))

    val callChunk = expression.target match {
      case CallTarget.MultiFunction(mf) => AsmChunk(regResult, PoemInstruction.Dispatch(regResult, mf, argumentRegs))

      case CallTarget.Value(expression) => ???
      case CallTarget.Constructor(binding) => ???

      case CallTarget.Dynamic(intrinsic) =>
        // TODO (assembly): If the Call has been analyzed to be unused, we can use `IntrinsicVoid`.
        AsmChunk(regResult, PoemInstruction.Intrinsic(regResult, intrinsic, argumentRegs))
    }

    AsmChunk.concat(arguments) ++ callChunk
  }

  override def visit(cond: Cond)(caseChunks: Vector[(AsmChunk, AsmChunk)]): AsmChunk = CondAssembler.generate(cond, caseChunks)

  override def visit(loop: WhileLoop)(conditionChunk: AsmChunk, bodyChunk: AsmChunk): AsmChunk = LoopAssembler.generate(loop, conditionChunk, bodyChunk)

  override def visit(loop: ForLoop)(collectionChunks: Vector[AsmChunk], bodyChunk: AsmChunk): AsmChunk = LoopAssembler.generate(loop, collectionChunks, bodyChunk)

  override def visit(expression: Ascription)(value: AsmChunk): AsmChunk = value

  override def before: PartialFunction[Expression, Unit] = {
    case expression: ForLoop =>
      // We have to assign registers to all extractor element variables so that the loop's body can access them.
      expression.extractors.foreach { extractor =>
        variableRegisterMap += (extractor.variable.uniqueKey -> registerProvider.fresh())
      }

    case _ =>
  }
}
