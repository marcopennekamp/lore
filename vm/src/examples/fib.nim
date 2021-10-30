from bytecode import Operation, Instruction, Function, Constants, new_instruction
from common import Example
from evaluator import init_frame_stats

let fib = Function(
  name: "fib",
  stack_size: 2,
  locals_size: 1,
  ref_stack_size: 2,
  ref_locals_size: 1,
  code: @[
    new_instruction(Operation.RefLocalLoad, 0, 0),
    new_instruction(Operation.IntUnbox, 0, 0),
    new_instruction(Operation.LocalStore, 0, 0),
    new_instruction(Operation.IntPush, 1, 0),
    new_instruction(Operation.LocalLoad, 0, 0),
    new_instruction(Operation.IntLessThan, 0, 0),
    new_instruction(Operation.JumpIfFalse, 19, 0),
    new_instruction(Operation.LocalLoad, 0, 0),
    new_instruction(Operation.IntPush, 1, 0),
    new_instruction(Operation.IntSubtract, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Dispatch, 1, 0),      # TODO (vm): We could also support direct/fixed calls which even bypass the "is single function" check.
    new_instruction(Operation.LocalLoad, 0, 0),
    new_instruction(Operation.IntPush, 2, 0),
    new_instruction(Operation.IntSubtract, 0, 0),
    new_instruction(Operation.IntBox, 0, 0),
    new_instruction(Operation.Dispatch, 1, 0),
    new_instruction(Operation.IntBoxAdd, 0, 0),
    new_instruction(Operation.Jump, 20, 0),
    new_instruction(Operation.RefLocalLoad, 0, 0),  # 19
    new_instruction(Operation.Return, 0, 0),        # 20
  ],
  constants: nil,
)
init_frame_stats(fib)

let constants = Constants(
  functions: @[fib],
)

fib.constants = constants

let test = Function(
  name: "test",
  stack_size: 0,
  locals_size: 0,
  ref_stack_size: 1,
  ref_locals_size: 0,
  code: @[
    new_instruction(Operation.IntBoxPush, 10, 0),
    new_instruction(Operation.Dispatch, 1, 0),
    new_instruction(Operation.Return, 0, 0),
  ],
  constants: constants,
)
init_frame_stats(test)

let example* = Example(
  name: "fib",
  function: test,
  arguments: @[],
  runs: 500_000,
)
