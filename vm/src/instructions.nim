type
  ## To describe an operation, you can use the following terms:
  ##  - `argx`: The xth uint16 argument of the instruction (starting with 0).
  ##  - `reg(x)`: Register x.
  ##  - `tpe(x)`: The xth entry in the types constants table.
  ##  - `val(x)`: The xth entry in the value constants table.
  ##  - `intr(x)`: The xth entry in the intrinsics constants table.
  ##  - `glb(x)`: The xth entry in the global variables constants table.
  ##  - `mfs(x)`: The xth entry in the multi functions constants table.
  Operation* {.pure.} = enum
    ## reg(arg0) <- val(arg1)
    Const

    ## reg(arg0) <- arg1 as int64
    IntConst

    ## reg(arg0) <- reg(arg1) + reg(arg2)
    IntAdd

    ## reg(arg0) <- reg(arg1) + arg2.int
    IntAddConst

    ## reg(arg0) <- reg(arg1) - arg2.int
    IntSubConst

    ## reg(arg0) <- reg(arg1) > arg2.int
    IntGtConst

    ## reg(arg0) <- reg(arg1) + reg(arg2)
    RealAdd

    ## Converts any TaggedValue to its native string representation. Reference values are not converted with
    ## `lore.core.to_string`. Rather, their pointer value is printed out. This operation is a fallback for `to_string`.
    ## reg(arg0) <- string_of(reg(arg1))
    StringOf

    ## reg(arg0) <- concat(reg(arg1), reg(arg2))
    StringConcat

    ## reg(arg0) <- concat(reg(arg1), val(arg2))
    StringConcatConst

    ## reg(arg0) <- concat(val(arg1), reg(arg2))
    StringConcatConstl

    ## reg(arg0) <- tuple(reg(arg1), reg(arg1 + 1), ..., reg(arg2))
    Tuple

    ## reg(arg0) <- tuple(reg(arg1), reg(arg2))
    Tuple2

    ## reg(arg0) <- reg(arg1).get(arg2)
    TupleGet

    ## reg(arg0) <- reg(arg1)()
    FunctionCall0

    ## reg(arg0) <- reg(arg1)(reg(arg2))
    FunctionCall1

    ## reg(arg0) <- reg(arg1)(reg(arg2), reg(arg3))
    FunctionCall2

    ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `tpe(arg3)`
    ListAppend

    ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type of `reg(arg1)`
    ListAppendUntyped

    ## reg(arg0) <- reg(arg1) == reg(arg2)
    SymbolEq

    ## reg(arg0) <- reg(arg1) == val(arg2)
    SymbolEqConst

    ## pc <- arg0
    Jump

    ## if !reg(arg1): pc <- arg0
    JumpIfFalse

    ## if reg(arg1): pc <- arg0
    JumpIfTrue

    ## reg(arg0) <- intr(arg1)(reg(arg2))
    Intrinsic1

    ## reg(arg0) <- intr(arg1)(frame, reg(arg2))
    IntrinsicFa1

    ## reg(arg0) <- intr(arg1)(reg(arg2), reg(arg3))
    Intrinsic2

    ## reg(arg0) <- intr(arg1)(frame, reg(arg2), reg(arg3))
    IntrinsicFa2

    ## Immediately gets the value of the global variable without checking whether it is initialized. This must only be
    ## used with eager global variables, or if it can be definitely proven that a lazy global variable must have been
    ## initialized at the point the instruction is used.
    ## reg(arg0) <- glb(arg1)
    GlobalGetEager

    ## reg(arg0) <- glb(arg1)
    GlobalGetLazy

    ## glb(arg0) <- reg(arg1)
    GlobalSet

    ## reg(arg0) <- mfs(arg1)(reg(arg2))
    Dispatch1

    ## reg(arg0) <- mfs(arg1)(reg(arg2), reg(arg3))
    Dispatch2

    ## return reg(arg0)
    Return

    ## return reg(0)
    Return0

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
