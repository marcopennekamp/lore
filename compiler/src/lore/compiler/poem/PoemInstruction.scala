package lore.compiler.poem

import lore.compiler.poem.Poem.Register
import lore.compiler.poem.PoemOperation.PoemOperation
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.DeclaredSchema

/**
  * A PoemInstruction is a representation of all possible VM instructions.
  *
  * The biggest difference to the Poem bytecode format is that the constant table will be assembled by the Poem writer.
  * Hence, a PoemInstruction doesn't contain index references to the constant table entries, but the constants
  * themselves.
  *
  * PoemInstructions may exist in an unprocessed form where `PLoc` doesn't refer to an absolute jump target, but an
  * instruction label. This will be resolved during assembly immediately after instructions for a function have been
  * flattened.
  */
sealed abstract class PoemInstruction(val operation: PoemOperation) {
  /**
    * All labels that have been attached to the instruction. The instruction's ultimate position will determine the
    * location of the label.
    */
  var labels: Vector[Poem.Label] = Vector.empty

  def addLabel(label: Poem.Label): Unit = {
    labels = labels :+ label
  }

  def withLabel(label: Poem.Label): PoemInstruction = {
    addLabel(label)
    this
  }

  override def toString: String = PoemInstruction.stringify(this)
}

object PoemInstruction {

  // These types are merely used to keep the following case class declarations short.
  type PReg = Poem.Register
  type PVal = PoemValue
  type PTpe = PoemType
  type PIntr = PoemIntrinsic
  type PSch = DeclaredSchema
  type PGlb = GlobalVariableDefinition
  type PMf = MultiFunctionDefinition
  type PMtsh = PoemMetaShape
  type PLoc = Poem.Location

  case class UnaryOperation(override val operation: PoemOperation, target: PReg, value: PReg) extends PoemInstruction(operation)
  case class BinaryOperation(override val operation: PoemOperation, target: PReg, a: PReg, b: PReg) extends PoemInstruction(operation)

  case class Assign(target: PReg, source: PReg) extends PoemInstruction(PoemOperation.Assign)

  case class Const(target: PReg, value: PVal) extends PoemInstruction(PoemOperation.Const)
  case class ConstPoly(target: PReg, value: PVal) extends PoemInstruction(PoemOperation.ConstPoly)

  case class IntConst(target: PReg, value: Int) extends PoemInstruction(PoemOperation.IntConst)
  case class IntToReal(target: PReg, value: PReg) extends PoemInstruction(PoemOperation.IntToReal)

  case class BooleanConst(target: PReg, value: Boolean) extends PoemInstruction(PoemOperation.BooleanConst)

  case class StringOf(target: PReg, value: PReg) extends PoemInstruction(PoemOperation.StringOf)
  case class StringConcat(target: PReg, a: PReg, b: PReg) extends PoemInstruction(PoemOperation.StringConcat)

  case class Tuple(target: PReg, elements: Vector[PReg]) extends PoemInstruction(PoemOperation.Tuple)
  case class TupleGet(target: PReg, tuple: PReg, index: Int) extends PoemInstruction(PoemOperation.TupleGet)

  def unit(target: PReg): PoemInstruction = Tuple(target, Vector.empty)

  case class FunctionCall(target: PReg, function: PReg, arguments: Vector[PReg]) extends PoemInstruction(PoemOperation.FunctionCall)

  case class List(target: PReg, tpe: PTpe, elements: Vector[PReg]) extends PoemInstruction(PoemOperation.List)
  case class ListAppend(override val operation: PoemOperation, target: PReg, list: PReg, element: PReg, tpe: PTpe) extends PoemInstruction(operation)
  case class ListAppendUntyped(target: PReg, list: PReg, element: PReg) extends PoemInstruction(PoemOperation.ListAppendUntyped)
  case class ListLength(target: PReg, list: PReg) extends PoemInstruction(PoemOperation.ListLength)
  case class ListGet(target: PReg, list: PReg, index: PReg) extends PoemInstruction(PoemOperation.ListGet)

  case class Shape(target: PReg, metaShape: PMtsh, properties: Vector[PReg]) extends PoemInstruction(PoemOperation.Shape)

  case class SymbolEq(target: PReg, a: PReg, b: PReg) extends PoemInstruction(PoemOperation.SymbolEq)

  case class Struct(target: PReg, schema: PSch, typeArguments: Vector[PReg], valueArguments: Vector[PReg]) extends PoemInstruction(PoemOperation.Struct)
  case class StructEq(target: PReg, a: PReg, b: PReg) extends PoemInstruction(PoemOperation.StructEq)

  sealed trait PropertyGetInstanceKind
  object PropertyGetInstanceKind {
    case object Any extends PropertyGetInstanceKind
    case object Shape extends PropertyGetInstanceKind
    case object Trait extends PropertyGetInstanceKind
    case class Struct(instanceSchema: PSch) extends PropertyGetInstanceKind
  }

  case class PropertyGet(
    target: PReg,
    instanceKind: PropertyGetInstanceKind,
    instance: PReg,
    propertyName: String,
  ) extends PoemInstruction(PoemOperation.PropertyGet)

  case class Jump(target: PLoc) extends PoemInstruction(PoemOperation.Jump)
  case class JumpIfFalse(target: PLoc, predicate: PReg) extends PoemInstruction(PoemOperation.JumpIfFalse)
  case class JumpIfTrue(target: PLoc, predicate: PReg) extends PoemInstruction(PoemOperation.JumpIfTrue)

  case class Intrinsic(target: PReg, intrinsic: PIntr, arguments: Vector[PReg]) extends PoemInstruction(PoemOperation.Intrinsic)
  case class IntrinsicVoid(intrinsic: PIntr, arguments: Vector[PReg]) extends PoemInstruction(PoemOperation.IntrinsicVoid)

  case class GlobalGet(target: PReg, global: PGlb) extends PoemInstruction(PoemOperation.GlobalGet)
  case class GlobalSet(global: PGlb, value: PReg) extends PoemInstruction(PoemOperation.GlobalSet)

  case class Dispatch(target: PReg, mf: PMf, arguments: Vector[PReg]) extends PoemInstruction(PoemOperation.Dispatch)

  case class Return(value: PReg) extends PoemInstruction(PoemOperation.Return)
  case class ReturnUnit() extends PoemInstruction(PoemOperation.ReturnUnit)
  case class Return0() extends PoemInstruction(PoemOperation.Return0)

  case class TypeArg(target: PReg, index: Int) extends PoemInstruction(PoemOperation.TypeArg)
  case class TypeConst(target: PReg, tpe: PTpe) extends PoemInstruction(PoemOperation.TypeConst)

  /**
    * Returns the instruction's jump target if it's a jump.
    */
  def getJumpTarget(instruction: PoemInstruction): Option[Poem.Location] = instruction match {
    case Jump(location) => Some(location)
    case JumpIfFalse(location, _) => Some(location)
    case JumpIfTrue(location, _) => Some(location)
    case _ => None
  }

  def isReturn(instruction: PoemInstruction): Boolean = instruction match {
    case Return(_) => true
    case ReturnUnit() => true
    case Return0() => true
    case _ => false
  }

  /**
    * Returns the number of registers that `instructions` require. This is simply the maximum accessed register + 1.
    */
  def registerCount(instructions: Vector[PoemInstruction]): Int = {
    def maximumRegister(instruction: PoemInstruction): Int = instruction match {
      case UnaryOperation(_, target, value) => Register.max(target, value)
      case BinaryOperation(_, target, a, b) => Register.max(target, a, b)
      case Assign(target, source) => Register.max(target, source)
      case Const(target, _) => target.id
      case ConstPoly(target, _) => target.id
      case IntConst(target, _) => target.id
      case IntToReal(target, value) => Register.max(target, value)
      case BooleanConst(target, _) => target.id
      case StringOf(target, value) => Register.max(target, value)
      case StringConcat(target, a, b) => Register.max(target, a, b)
      case Tuple(target, elements) => Register.max(target, elements)
      case TupleGet(target, tuple, _) => Register.max(target, tuple)
      case FunctionCall(target, function, arguments) => Register.max(target, function, arguments)
      case List(target, _, elements) => Register.max(target, elements)
      case ListAppend(_, target, list, element, _) => Register.max(target, list, element)
      case ListAppendUntyped(target, list, element) => Register.max(target, list, element)
      case ListLength(target, list) => Register.max(target, list)
      case ListGet(target, list, index) => Register.max(target, list, index)
      case Shape(target, _, properties) => Register.max(target, properties)
      case SymbolEq(target, a, b) => Register.max(target, a, b)
      case Struct(target, _, typeArguments, valueArguments) => Register.max(target, typeArguments ++ valueArguments)
      case StructEq(target, a, b) => Register.max(target, a, b)
      case PropertyGet(target, _, instance, _) => Register.max(target, instance)
      case Jump(_) => -1
      case JumpIfFalse(_, predicate) => predicate.id
      case JumpIfTrue(_, predicate) => predicate.id
      case Intrinsic(target, _, arguments) => Register.max(target, arguments)
      case IntrinsicVoid(_, arguments) => Register.max(arguments)
      case GlobalGet(target, _) => target.id
      case GlobalSet(_, value) => value.id
      case Dispatch(target, _, arguments) => Register.max(target, arguments)
      case Return(value) => value.id
      case ReturnUnit() => -1
      case Return0() => 0
      case TypeArg(target, _) => target.id
      case TypeConst(target, _) => target.id
    }

    if (instructions.isEmpty) 0
    else instructions.map(maximumRegister).max + 1
  }

  def mapRegisters(
    instruction: PoemInstruction,
    applyTarget: Poem.Register => Poem.Register,
    applySource: Poem.Register => Poem.Register,
  ): PoemInstruction = instruction match {
    case instruction@UnaryOperation(_, target, value) => instruction.copy(target = applyTarget(target), value = applySource(value))
    case instruction@BinaryOperation(_, target, a, b) => instruction.copy(target = applyTarget(target), a = applySource(a), b = applySource(b))
    case instruction@Assign(target, source) => instruction.copy(target = applyTarget(target), source = applySource(source))
    case instruction@Const(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@ConstPoly(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@IntConst(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@IntToReal(target, value) => instruction.copy(target = applyTarget(target), value = applySource(value))
    case instruction@BooleanConst(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@StringOf(target, value) => instruction.copy(target = applyTarget(target), value = applySource(value))
    case instruction@StringConcat(target, a, b) => instruction.copy(target = applyTarget(target), a = applySource(a), b = applySource(b))
    case instruction@Tuple(target, elements) => instruction.copy(target = applyTarget(target), elements = elements.map(applySource))
    case instruction@TupleGet(target, tuple, _) => instruction.copy(target = applyTarget(target), tuple = applySource(tuple))
    case instruction@FunctionCall(target, function, arguments) => instruction.copy(target = applyTarget(target), function = applySource(function), arguments = arguments.map(applySource))
    case instruction@List(target, _, elements) => instruction.copy(target = applyTarget(target), elements = elements.map(applySource))
    case instruction@ListAppend(_, target, list, element, _) => instruction.copy(target = applyTarget(target), list = applySource(list), element = applySource(element))
    case instruction@ListAppendUntyped(target, list, element) => instruction.copy(target = applyTarget(target), list = applySource(list), element = applySource(element))
    case instruction@ListLength(target, list) => instruction.copy(target = applyTarget(target), list = applySource(list))
    case instruction@ListGet(target, list, index) => instruction.copy(target = applyTarget(target), list = applySource(list), index = applySource(index))
    case instruction@Shape(target, _, properties) => instruction.copy(target = applyTarget(target), properties = properties.map(applySource))
    case instruction@SymbolEq(target, a, b) => instruction.copy(target = applyTarget(target), a = applySource(a), b = applySource(b))
    case instruction@Struct(target, _, typeArguments, valueArguments) => instruction.copy(target = applyTarget(target), typeArguments = typeArguments.map(applySource), valueArguments = valueArguments.map(applySource))
    case instruction@StructEq(target, a, b) => instruction.copy(target = applyTarget(target), a = applySource(a), b = applySource(b))
    case instruction@PropertyGet(target, _, instance, _) => instruction.copy(target = applyTarget(target), instance = applySource(instance))
    case instruction@Jump(_) => instruction
    case instruction@JumpIfFalse(_, predicate) => instruction.copy(predicate = applySource(predicate))
    case instruction@JumpIfTrue(_, predicate) => instruction.copy(predicate = applySource(predicate))
    case instruction@Intrinsic(target, _, arguments) => instruction.copy(target = applyTarget(target), arguments = arguments.map(applySource))
    case instruction@IntrinsicVoid(_, arguments) => instruction.copy(arguments = arguments.map(applySource))
    case instruction@GlobalGet(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@GlobalSet(_, value) => instruction.copy(value = applySource(value))
    case instruction@Dispatch(target, _, arguments) => instruction.copy(target = applyTarget(target), arguments = arguments.map(applySource))
    case instruction@Return(value) => instruction.copy(value = applySource(value))
    case instruction@ReturnUnit() => instruction
    case instruction@Return0() => instruction
    case instruction@TypeArg(target, _) => instruction.copy(target = applyTarget(target))
    case instruction@TypeConst(target, _) => instruction.copy(target = applyTarget(target))
  }

  case class DefUseInfo(definitions: Vector[Poem.Register], uses: Vector[Poem.Register])

  /**
    * Returns the register definition and use sets for the given instruction.
    *
    *   - `r in def(inst)`: The instruction `inst` assigns a new value to register `r`.
    *   - `r in use(inst)`: The instruction `inst` uses the register `r`.
    *
    * This is used by liveness analysis during register allocation.
    *
    * TODO (assembly): Rename this to target and source sets? (Still call it def/use in liveliness.)
    */
  def defUseInfo(instruction: PoemInstruction): DefUseInfo = {
    val (defList, useList) = instruction match {
      case UnaryOperation(_, target, value) => (Vector(target), Vector(value))
      case BinaryOperation(_, target, a, b) => (Vector(target), Vector(a, b))
      case Assign(target, source) => (Vector(target), Vector(source))
      case Const(target, _) => (Vector(target), Vector.empty)
      case ConstPoly(target, _) => (Vector(target), Vector.empty)
      case IntConst(target, _) => (Vector(target), Vector.empty)
      case IntToReal(target, value) => (Vector(target), Vector(value))
      case BooleanConst(target, _) => (Vector(target), Vector.empty)
      case StringOf(target, value) => (Vector(target), Vector(value))
      case StringConcat(target, a, b) => (Vector(target), Vector(a, b))
      case Tuple(target, elements) => (Vector(target), elements)
      case TupleGet(target, tuple, _) => (Vector(target), Vector(tuple))
      case FunctionCall(target, function, arguments) => (Vector(target), function +: arguments)
      case List(target, _, elements) => (Vector(target), elements)
      case ListAppend(_, target, list, element, _) => (Vector(target), Vector(list, element))
      case ListAppendUntyped(target, list, element) => (Vector(target), Vector(list, element))
      case ListLength(target, list) => (Vector(target), Vector(list))
      case ListGet(target, list, index) => (Vector(target), Vector(list, index))
      case Shape(target, _, properties) => (Vector(target), properties)
      case SymbolEq(target, a, b) => (Vector(target), Vector(a, b))
      case Struct(target, _, typeArguments, valueArguments) => (Vector(target), typeArguments ++ valueArguments)
      case StructEq(target, a, b) => (Vector(target), Vector(a, b))
      case PropertyGet(target, _, instance, _) => (Vector(target), Vector(instance))
      case Jump(_) => (Vector.empty, Vector.empty)
      case JumpIfFalse(_, predicate) => (Vector.empty, Vector(predicate))
      case JumpIfTrue(_, predicate) => (Vector.empty, Vector(predicate))
      case Intrinsic(target, _, arguments) => (Vector(target), arguments)
      case IntrinsicVoid(_, arguments) => (Vector.empty, arguments)
      case GlobalGet(target, _) => (Vector(target), Vector.empty)
      case GlobalSet(_, value) => (Vector.empty, Vector(value))
      case Dispatch(target, _, arguments) => (Vector(target), arguments)
      case Return(value) => (Vector.empty, Vector(value))
      case ReturnUnit() => (Vector.empty, Vector.empty)
      case Return0() => (Vector.empty, Vector(Poem.Register(0)))
      case TypeArg(target, _) => (Vector(target), Vector.empty)
      case TypeConst(target, _) => (Vector(target), Vector.empty)
    }
    DefUseInfo(defList.distinct, useList.distinct)
  }

  /**
    * Returns a human-readable representation of the instruction for debugging.
    */
  def stringify(instruction: PoemInstruction): String = {
    s"${instruction.operation} " + (instruction match {
      case UnaryOperation(_, target, value) => s"$target <- $value"
      case BinaryOperation(_, target, a, b) => s"$target <- $a $b"
      case Assign(target, source) => s"$target <- $source"
      case Const(target, value) => s"$target <- $value"
      case ConstPoly(target, value) => s"$target <- $value"
      case IntConst(target, value) => s"$target <- $value"
      case IntToReal(target, value) => s"$target <- $value as Real"
      case BooleanConst(target, value) => s"$target <- $value"
      case StringOf(target, value) => s"$target <- string_of($value)"
      case StringConcat(target, a, b) => s"$target <- concat($a, $b)"
      case Tuple(target, elements) => s"$target <- tuple(${elements.mkString(", ")})"
      case TupleGet(target, tuple, index) => s"$target <- $tuple[$index]"
      case FunctionCall(target, function, arguments) => s"$target <- $function(${arguments.mkString(", ")})"
      case List(target, tpe, elements) => s"$target <- list(${elements.mkString(", ")}) with type $tpe"
      case ListAppend(_, target, list, element, tpe) => s"$target <- $list :+ $element with type $tpe"
      case ListAppendUntyped(target, list, element) => s"$target <- $list :+ $element"
      case ListLength(target, list) => s"$target <- $list.length"
      case ListGet(target, list, index) => s"$target <- $list[$index]"
      case Shape(target, metaShape, properties) => s"$target <- shape($metaShape, ${properties.mkString(", ")})"
      case SymbolEq(target, a, b) => s"$target <- $a $b"
      case Struct(target, schema, typeArguments, valueArguments) => s"$target <- ${schema.name}[${typeArguments.mkString(", ")}](${valueArguments.mkString(", ")})"
      case StructEq(target, a, b) => s"$target <- $a $b"
      case PropertyGet(target, _, instance, propertyName) => s"$target <- $instance[$propertyName]"
      case Jump(target) => s"$target"
      case JumpIfFalse(target, predicate) => s"$target if !$predicate"
      case JumpIfTrue(target, predicate) => s"$target if $predicate"
      case Intrinsic(target, intrinsic, arguments) => s"$target <- ${intrinsic.name}(${arguments.mkString(", ")})"
      case IntrinsicVoid(intrinsic, arguments) => s"${intrinsic.name}(${arguments.mkString(", ")})"
      case GlobalGet(target, global) => s"$target <- ${global.name}"
      case GlobalSet(global, value) => s"${global.name} <- $value"
      case Dispatch(target, mf, arguments) => s"$target <- ${mf.name}(${arguments.mkString(", ")})"
      case Return(value) => s"$value"
      case ReturnUnit() => s""
      case Return0() => s""
      case TypeArg(target, index) => s"$target <- targ($index)"
      case TypeConst(target, tpe) => s"$target <- $tpe"
    })
  }

}
