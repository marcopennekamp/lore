type
  ## To describe an operation, you can use the following terms:
  ##  - `argx`: The xth uint16 argument of the instruction (starting with 0).
  ##  - `reg(x)`: Register x.
  ##  - `tpe(x)`: The xth entry in the types constants table.
  ##  - `mf(x)`: The xth entry in the multi functions constants table.
  ##  - `val(x)`: The xth entry in the value constants table.
  Operation* {.pure.} = enum
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

    ## pc <- arg0
    Jump

    ## if !reg(arg1): pc <- arg0
    JumpIfFalse

    ## if reg(arg1): pc <- arg0
    JumpIfTrue

    ## arg0: Target register.
    ## arg1: Constants table ID of the multi-function.
    ## arg2: Argument 1 register.
    ## reg(arg0) <- mfs(arg1)(reg(arg2))
    Dispatch1

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

  # TODO (vm): Maybe turn the arguments into a fixed-size array.
  Instruction* = object
    operation*: Operation
    arguments*: array[3, Argument]

proc arg*(instruction: Instruction, index: uint16): uint16 = instruction.arguments[index].uint_value
proc argi*(instruction: Instruction, index: uint16): int16 = instruction.arguments[index].int_value

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16): Instruction =
  Instruction(
    operation: operation,
    arguments: [
      Argument(uint_value: arg0),
      Argument(uint_value: arg1),
      Argument(uint_value: arg2),
    ],
  )

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction = new_instruction(operation, arg0, arg1, 0)
proc new_instruction*(operation: Operation, arg0: uint16): Instruction = new_instruction(operation, arg0, 0)
proc new_instruction*(operation: Operation): Instruction = new_instruction(operation, 0)
