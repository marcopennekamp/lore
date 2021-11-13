type
  ## To describe an operation, you can use the following terms:
  ##  - `argx`: The xth uint16 argument of the instruction (starting with 0).
  ##  - `reg(x)`: Register x.
  ##  - `tpe(x)`: The xth entry in the types constants table.
  ##  - `val(x)`: The xth entry in the value constants table.
  ##  - `mfs(x)`: The xth entry in the multi functions constants table.
  ##
  ## Please note that the `FunctionCall` and `Dispatch` operations must have the same argument registers, because of
  ## the evaluator implementation.
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

    ## reg(arg0) <- mfs(arg1)(reg(arg2))
    Dispatch1

    ## reg(arg0) <- mfs(arg1)(reg(arg2), reg(arg3))
    Dispatch2

    ## return reg(arg0)
    Return

    ## return reg(0)
    Return0

    # TODO (vm): Implement intrinsic calls to support native list functions and so on.
    ## arg0: Target register.
    ## arg1: Constants table ID of the intrinsic function.
    ## arg2: Argument 1 register.
    #CallIntrinsic1

  Argument {.union.} = object
    uint_value*: uint16
    int_value*: int16

  Instruction* = object
    operation*: Operation
    arguments*: array[4, Argument]

proc arg*(instruction: Instruction, index: uint16): uint16 = instruction.arguments[index].uint_value
proc argi*(instruction: Instruction, index: uint16): int16 = instruction.arguments[index].int_value

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16): Instruction =
  Instruction(
    operation: operation,
    arguments: [
      Argument(uint_value: arg0),
      Argument(uint_value: arg1),
      Argument(uint_value: arg2),
      Argument(uint_value: arg3),
    ],
  )

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16): Instruction = new_instruction(operation, arg0, arg1, arg2, 0)
proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction = new_instruction(operation, arg0, arg1, 0)
proc new_instruction*(operation: Operation, arg0: uint16): Instruction = new_instruction(operation, arg0, 0)
proc new_instruction*(operation: Operation): Instruction = new_instruction(operation, 0)
