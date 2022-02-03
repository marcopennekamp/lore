package lore.compiler.poem

/**
  * A VM operation code. The order of the operations must be exactly equal to the VM, so that the correct IDs are
  * assigned.
  */
object PoemOperation extends Enumeration {
  type PoemOperation = Value

  val Const, ConstPoly = Value
  val IntConst, IntAdd, IntAddConst, IntSubConst, IntLt, IntLtConst, IntGtConst = Value
  val RealAdd = Value
  val StringOf, StringConcat = Value
  val Tuple, TupleGet = Value
  val FunctionCall = Value
  val ListAppend, ListAppendPoly, ListAppendUntyped = Value
  val Shape = Value
  val SymbolEq = Value
  val Struct, StructEq = Value
  val PropertyGet = Value
  val Jump, JumpIfFalse, JumpIfTrue = Value
  val Intrinsic, IntrinsicVoid = Value
  val GlobalGet, GlobalSet = Value
  val Dispatch = Value
  val Return, ReturnUnit, Return0 = Value
  val TypeArg, TypeConst = Value
}
