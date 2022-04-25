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
  val BooleanConst, BooleanNot, BooleanOr, BooleanAnd, BooleanEq = Value
  val StringOf, StringConcat, StringEq, StringLt, StringLte = Value
  val Tuple, TupleGet = Value
  val FunctionCall, FunctionSingle, FunctionLambda, LambdaLocal = Value
  val List, ListAppend, ListAppendUntyped, ListLength, ListGet = Value
  val Shape = Value
  val SymbolEq = Value
  val Struct, StructPoly, StructEq = Value
  val PropertyGet, PropertySet = Value
  val Jump, JumpIfFalse, JumpIfTrue = Value
  val Intrinsic = Value
  val GlobalGet, GlobalSet = Value
  val Dispatch, Call, CallPoly = Value
  val Return = Value
  val TypeArg, TypeConst, TypeOf, TypePathIndex, TypePathProperty, TypePathTypeArgument = Value
}
