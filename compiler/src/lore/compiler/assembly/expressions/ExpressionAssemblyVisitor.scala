package lore.compiler.assembly.expressions

import lore.compiler.assembly.types.PoemTypeAssembler
import lore.compiler.assembly.{AsmChunk, PropertyOrder, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem
import lore.compiler.poem.PoemInstruction.PropertyGetInstanceKind
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.{Expression, ExpressionVisitor}
import lore.compiler.types.Type.isPolymorphic
import lore.compiler.types._

import scala.collection.immutable.HashMap

// TODO (assembly): Remember to insert implicit conversions from Int to Real values for arithmetic and comparison expressions.

/**
  * The visitor should generate Jump instructions with <i>relative</i> locations. They will later be converted to
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
    PoemValueAssembler.generateConst(expression, target).getOrElse {
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
    PoemValueAssembler.generateConst(expression, target).getOrElse {
      // TODO (assembly): Add a List instruction. The first argument should be the list's type so that the instruction
      //                  doesn't construct the type on the fly. `List` should support more than 256 arguments, which
      //                  is the current upper bound for operand lists. To support this, we can have the VM generate
      //                  `ListAppendUntyped` instructions from the `List` instruction to handle the overflow in those
      //                  extremely rare cases.
      val instruction = ??? // PoemInstruction.List(target, values.map(_.forceResult(expression.position)))
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
    PoemValueAssembler.generateConst(expression, target).getOrElse {
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
    val operation = if (isPolymorphic(expression.tpe)) PoemOperation.ListAppendPoly else PoemOperation.ListAppend
    val target = registerProvider.fresh()
    val list = listChunk.forceResult(expression.position)
    val element = elementChunk.forceResult(expression.position)
    val tpe = PoemTypeAssembler.generate(expression.tpe)
    val instruction = PoemInstruction.ListAppend(operation, target, list, element, tpe)
    listChunk ++ elementChunk ++ AsmChunk(target, instruction)
  }

  override def visit(expression: XaryOperation)(operands: Vector[AsmChunk]): AsmChunk = {
    PrimitiveOperationAssembler.generateXaryOperation(expression, operands)
  }

  override def visit(expression: Call)(target: Option[AsmChunk], arguments: Vector[AsmChunk]): AsmChunk = {
//    def withArguments(f: Vector[Target.TargetExpression] => Target.TargetExpression) = {
//      AsmChunk.combine(arguments) { arguments => AsmChunk.expression(f(arguments)) }
//    }
//    def functionValueCall(function: Target.TargetExpression) = withArguments(RuntimeApi.functions.call(function, _))
//    def directCall(expression: Target.TargetExpression) = withArguments(Target.Call(expression, _))
//
//    expression.target match {
//      case CallTarget.Value(ConstructorValue(_, structType, _)) =>
//        // Optimization: If we're directly calling a constructor value, the function call boils down to calling the
//        // `construct` function. This allows us to bypass a run-time call to `getConstructor` for structs with type
//        // parameters.
//        withArguments(arguments => InstantiationTranspiler.transpileStructInstantiation(structType, arguments))
//
//      case CallTarget.Value(_) => target.get.flatMap(functionValueCall)
//
//      case CallTarget.MultiFunction(mf) => directCall(TargetRepresentableTranspiler.transpile(mf))
//
//      case CallTarget.Constructor(_) =>
//        // The result type of the constructor call is the struct with instantiated type parameters. Hence, we can take
//        // it here for transpilation.
//        val structType = expression.tpe match {
//          case structType: StructType => structType
//          case _ => throw CompilationException(s"The result type of a constructor must be a struct. Position: ${expression.position}.")
//        }
//        withArguments(arguments => InstantiationTranspiler.transpileStructInstantiation(structType, arguments))
//
//      case CallTarget.Dynamic(name) => directCall(name.asVariable)
//    }
    ???
  }

  override def visit(expression: Cond)(cases: Vector[(AsmChunk, AsmChunk)]): AsmChunk = ??? // ConditionalTranspiler.transpile(expression, cases)

  override def visit(loop: WhileLoop)(condition: AsmChunk, body: AsmChunk): AsmChunk = ??? // LoopTranspiler().transpile(loop, condition, body)

  override def visit(loop: ForLoop)(collections: Vector[AsmChunk], body: AsmChunk): AsmChunk = ??? // LoopTranspiler().transpile(loop, collections, body)

  override def visit(expression: Ascription)(value: AsmChunk): AsmChunk = value
}
