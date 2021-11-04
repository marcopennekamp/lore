type
  Operation* {.pure.} = enum
    IntConst
    IntAdd
    IntAddConst
    #IntSub
    IntSubConst
    #IntLt
    IntGtConst

    Jump
    JumpIfFalse
    JumpIfTrue

    ## arg0: Target register.
    ## arg1: Constant ID of the multi-function to call.
    ## arg2: Argument 1 register.
    Dispatch1

    ## arg0: Return value register.
    Return

    ## `Return0` returns the value from register 0.
    Return0

  Argument {.union.} = object
    uint_value*: uint16
    int_value*: int16

  # TODO (vm): Maybe turn the arguments into a fixed-size array.
  Instruction* = object
    operation*: Operation
    arg0*: Argument
    arg1*: Argument
    arg2*: Argument

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16): Instruction =
  Instruction(
    operation: operation,
    arg0: Argument(uint_value: arg0),
    arg1: Argument(uint_value: arg1),
    arg2: Argument(uint_value: arg2),
  )

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction = new_instruction(operation, arg0, arg1, 0)
proc new_instruction*(operation: Operation, arg0: uint16): Instruction = new_instruction(operation, arg0, 0)
proc new_instruction*(operation: Operation): Instruction = new_instruction(operation, 0)
