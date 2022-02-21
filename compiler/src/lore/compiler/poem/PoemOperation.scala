package lore.compiler.poem

/**
  * A VM operation code. The order of the operations must be exactly equal to the VM, so that the correct IDs are
  * assigned.
  */
object PoemOperation extends Enumeration {
  type PoemOperation = Value

  val Assign = Value
  val Const = Value
  val IntConst, IntNeg, IntAdd, IntSub, IntMul, IntDiv, IntEq, IntLt, IntLte, IntToReal = Value
  val RealNeg, RealAdd, RealSub, RealMul, RealDiv, RealEq, RealLt, RealLte = Value
  val BooleanConst, BooleanNot, BooleanOr, BooleanAnd = Value
  val StringOf, StringConcat, StringEq, StringLt, StringLte = Value
  val Tuple, TupleGet = Value
  val FunctionCall, Lambda, LambdaLocal = Value
  val List, ListAppend, ListAppendUntyped, ListLength, ListGet = Value
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
