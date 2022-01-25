type
  Operation* {.pure.} = enum
    ## To describe an operation, you can use the following terms:
    ##  - `argx`: The xth uint16 argument of the instruction (starting with 0).
    ##  - `reg(x)`: Register x.
    ##  - `tpe(x)`: The xth entry in the types constants table.
    ##  - `val(x)`: The xth entry in the values constants table.
    ##  - `nam(x)`: The xth entry in the names constants table.
    ##  - `intr(x)`: The xth entry in the intrinsics constants table.
    ##  - `glb(x)`: The xth entry in the global variables constants table.
    ##  - `mfs(x)`: The xth entry in the multi functions constants table.
    ##  - `mtsh(x)`: The xth entry in the meta shapes constants table.
    ##  - `substitute(t)`: Substitutes the current function instance's type arguments into type `t`.
    ##  - `substitute_types(v)`: Substitutes the current function instance's type arguments into value `v`.

    Const
      ## reg(arg0) <- val(arg1)

    ConstPoly
      ## reg(arg0) <- substitute_types(val(arg1))

    IntConst
      ## reg(arg0) <- arg1 as int64

    IntAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    IntAddConst
      ## reg(arg0) <- reg(arg1) + arg2.int

    IntSubConst
      ## reg(arg0) <- reg(arg1) - arg2.int

    IntLt
      ## reg(arg0) <- reg(arg1) < reg(arg2)

    IntLtConst
      ## reg(arg0) <- reg(arg1) < arg2.int

    IntGtConst
      ## reg(arg0) <- reg(arg1) > arg2.int

    RealAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    StringOf
      ## Converts any TaggedValue to its native string representation. Reference values are not converted with
      ## `lore.core.to_string`. Rather, their pointer value is printed out. This operation is a fallback for `to_string`.
      ## reg(arg0) <- string_of(reg(arg1))

    StringConcat
      ## reg(arg0) <- concat(reg(arg1), reg(arg2))

    StringConcatConst
      ## reg(arg0) <- concat(reg(arg1), val(arg2))

    StringConcatConstl
      ## reg(arg0) <- concat(val(arg1), reg(arg2))

    Tuple
      ## reg(arg0) <- tuple(reg(arg1), reg(arg1 + 1), ..., reg(arg2))

    Tuple2
      ## reg(arg0) <- tuple(reg(arg1), reg(arg2))

    TupleGet
      ## reg(arg0) <- reg(arg1).get(arg2)

    FunctionCall0
      ## reg(arg0) <- reg(arg1)()

    FunctionCall1
      ## reg(arg0) <- reg(arg1)(reg(arg2))

    FunctionCall2
      ## reg(arg0) <- reg(arg1)(reg(arg2), reg(arg3))

    ListAppend
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `tpe(arg3)`

    ListAppendPoly
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `substitute(tpe(arg3))`

    ListAppendUntyped
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type of `reg(arg1)`

    Shape
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2), reg(arg2 + 1), ..., reg(arg3))

    Shape1
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2))

    Shape2
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2), reg(arg3))

    ShapeGetProperty
      ## reg(arg0) <- reg(arg1)[nam(arg2)]

    SymbolEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    SymbolEqConst
      ## reg(arg0) <- reg(arg1) == val(arg2)

    Jump
      ## pc <- arg0

    JumpIfFalse
      ## if !reg(arg1): pc <- arg0

    JumpIfTrue
      ## if reg(arg1): pc <- arg0

    Intrinsic0
      ## reg(arg0) <- intr(arg1)()

    IntrinsicVoid0
      ## intr(arg0)()

    Intrinsic1
      ## reg(arg0) <- intr(arg1)(reg(arg2))

    IntrinsicFa1
      ## reg(arg0) <- intr(arg1)(frame, reg(arg2))

    IntrinsicVoid1
      ## intr(arg0)(reg(arg1))

    Intrinsic2
      ## reg(arg0) <- intr(arg1)(reg(arg2), reg(arg3))

    IntrinsicFa2
      ## reg(arg0) <- intr(arg1)(frame, reg(arg2), reg(arg3))

    IntrinsicVoidFa2
      ## intr(arg0)(frame, reg(arg1), reg(arg2))

    GlobalGetEager
      ## Immediately gets the value of the global variable without checking whether it is initialized. This must only be
      ## used with eager global variables, or if it can be definitely proven that a lazy global variable must have been
      ## initialized at the point the instruction is used.
      ## reg(arg0) <- glb(arg1)

    GlobalGetLazy
      ## reg(arg0) <- glb(arg1)

    GlobalSet
      ## glb(arg0) <- reg(arg1)

    Dispatch1
      ## reg(arg0) <- mfs(arg1)(reg(arg2))

    Dispatch2
      ## reg(arg0) <- mfs(arg1)(reg(arg2), reg(arg3))

    Return
      ## return reg(arg0)

    ReturnUnit
      ## return unit

    Return0
      ## return reg(0)

  Argument = distinct uint16

  Instruction* = object
    operation*: Operation
    arguments*: array[4, Argument]

template arg*(instruction: Instruction, index: uint16): uint16 = cast[uint16](instruction.arguments[index])
template argi*(instruction: Instruction, index: uint16): int16 = cast[int16](instruction.arguments[index])

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16): Instruction =
  Instruction(
    operation: operation,
    arguments: [
      Argument(arg0),
      Argument(arg1),
      Argument(arg2),
      Argument(arg3),
    ],
  )

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16): Instruction = new_instruction(operation, arg0, arg1, arg2, 0)
proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction = new_instruction(operation, arg0, arg1, 0)
proc new_instruction*(operation: Operation, arg0: uint16): Instruction = new_instruction(operation, arg0, 0)
proc new_instruction*(operation: Operation): Instruction = new_instruction(operation, 0)
