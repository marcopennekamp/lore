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
      case List(target, tpe, elements) => Register.max(target, elements)
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

}
