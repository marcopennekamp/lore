package lore.compiler.poem
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
  * PoemInstructions may exist in an unprocessed form where `PLoc` doesn't refer to an absolute jump target, but a
  * relative one. This will be resolved during assembly immediately after instructions for a function have been
  * flattened.
  */
sealed abstract class PoemInstruction(val operation: PoemOperation)

object PoemInstruction {
  // These types are merely used to keep the following case class declarations short.
  type PReg = Poem.Register
  type PVal = PoemValue
  type PTpe = PoemType
  type PIntr = String
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

  case class FunctionCall(target: PReg, function: PReg, arguments: Vector[PReg]) extends PoemInstruction(PoemOperation.FunctionCall)

  // TODO (assembly): Can't we decide whether to use Poly right here? If `tpe` contains a type variable, it must be Poly, no?
  //                  We could even decide this inside the VM... This might be an implementation detail that shouldn't
  //                  be exposed by the API.
  case class ListAppend(target: PReg, list: PReg, element: PReg, tpe: PTpe) extends PoemInstruction(PoemOperation.ListAppend)
  case class ListAppendPoly(target: PReg, list: PReg, element: PReg, tpe: PTpe) extends PoemInstruction(PoemOperation.ListAppendPoly)
  case class ListAppendUntyped(target: PReg, list: PReg, element: PReg) extends PoemInstruction(PoemOperation.ListAppendUntyped)

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
}
