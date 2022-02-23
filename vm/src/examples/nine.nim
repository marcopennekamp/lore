import "../poems"

let nine = PoemFunction(
  name: "nine",
  input_type: poem_unit_type,
  output_type: poem_int_type,
  is_abstract: false,
  register_count: 2,
  instructions: @[
    poem_inst(PoemOperation.IntConst, 0, 1),
    poem_inst(PoemOperation.IntConst, 1, 2),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst(PoemOperation.IntConst, 1, 3),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst(PoemOperation.IntConst, 1, 4),
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst(PoemOperation.IntConst, 1, 0xffff), # 0xffff is -1 as an int16.
    poem_inst(PoemOperation.IntAdd, 0, 0, 1),
    poem_inst_return(0),
  ],
)

let poem* = Poem(
  constants: PoemConstants(),
  functions: @[nine],
)
