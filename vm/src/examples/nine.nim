from common import Example
from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from instructions import Operation, Instruction, new_instruction

from "../poems" import Poem, PoemConstants, PoemFunction, PoemXaryType, PoemBasicType
from "../types" import Kind

let nine = PoemFunction(
  name: "nine",
  input_type: PoemXaryType(kind: Kind.Tuple, types: @[]),
  output_type: PoemBasicType(tpe: types.int),
  register_count: 1,
  instructions: @[
    new_instruction(Operation.IntConst, 0, 1),
    new_instruction(Operation.IntAddConst, 0, 0, 2),
    new_instruction(Operation.IntAddConst, 0, 0, 3),
    new_instruction(Operation.IntAddConst, 0, 0, 4),
    new_instruction(Operation.IntAddConst, 0, 0, 0xffff),  # 0xffff is -1 as an int16.
    new_instruction(Operation.Return0),
  ],
)

# runs: 50_000_000
let poem* = Poem(
  constants: PoemConstants(
    multi_functions: @[],
  ),
  functions: @[nine],
)
