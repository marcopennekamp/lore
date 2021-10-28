type
  Operation* {.pure.} = enum
    ArgumentLoad

    LocalStore
    LocalLoad

    IntPush
    IntAdd
    IntSubtract
    IntLessThan

    IntBox
    IntUnbox
    IntBoxPush
    IntBoxAdd

    Jump
    JumpIfFalse
    JumpIfTrue

    # arg0: Number of arguments to pop off the stack.
    # arg1: Constant ID of the multi-function to call.
    Dispatch

    Return

  Argument {.union.} = object
    uint_value*: uint16
    int_value*: int16

  Instruction* = object
    operation*: Operation
    arg0*: Argument
    arg1*: Argument

  # A Constants object provides quick access to predefined types, values, and functions.
  Constants* = ref object
    # TODO (vm): Add types and values.
    functions*: seq[Function] # TODO (vm): This should point to the multi-function, of course, once we implement them.

  Function* = ref object
    name*: string
    # TODO (vm): Add the function's signature.
    arguments_count*: uint16
    locals_count*: uint16
    code*: seq[Instruction]
    # The `constants` object will be initialized after all type, value, and function constants have been resolved.
    constants*: Constants

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction =
  Instruction(operation: operation, arg0: Argument(uint_value: arg0), arg1: Argument(uint_value: arg1))
