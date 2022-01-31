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
  val StringOf, StringConcat, StringConcatConst, StringConcatConstl = Value
  val Tuple, Tuple2, TupleGet = Value
  val FunctionCall0, FunctionCall1, FunctionCall2 = Value
  val ListAppend, ListAppendPoly, ListAppendUntyped = Value
  val Shape, Shape1, Shape2, ShapeGetProperty = Value
  val SymbolEq, SymbolEqConst = Value
  val Struct, Struct1, Struct2, StructPoly, StructGetProperty, StructGetNamedProperty, StructEq = Value
  val Jump, JumpIfFalse, JumpIfTrue = Value
  val Intrinsic0, Intrinsic1, Intrinsic2, IntrinsicVoid0, IntrinsicVoid1, IntrinsicFa1, IntrinsicFa2,
      IntrinsicVoidFa2 = Value
  val GlobalGetEager, GlobalGetLazy, GlobalSet = Value
  val Dispatch1, Dispatch2 = Value
  val Return, ReturnUnit, Return0 = Value
  val TypeArg, TypeConst = Value
}
