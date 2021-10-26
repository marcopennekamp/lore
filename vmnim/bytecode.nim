type
  Operation* {.pure.} = enum
    IntAdd
    IntPush
    Return

  Argument {.union.} = object
    uint_value*: uint16
    int_value*: int16

  Instruction* = object
    operation*: Operation
    arg0*: Argument
    arg1*: Argument

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction =
  Instruction(operation: operation, arg0: Argument(uint_value: arg0), arg1: Argument(uint_value: arg0))

#[
const int_mask: int16 = 0

proc arg0_int*(instruction: Instruction): int =
  var n: int = 0
  copy_mem

  cast[int](bitor(int_mask, instruction.arg0))
  #cast[int](instruction.arg0)
]#
