import std/macros
import std/strformat

import definitions
from dispatch import find_dispatch_target_from_arguments
import imseqs
import instructions
from types import Type, StructSchema, StructType, substitute
import values
from utils import when_debug

proc evaluate(frame: FramePtr)
proc evaluate*(entry_function: ptr FunctionInstance, frame_mem: pointer): TaggedValue

proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, arguments: open_array[TaggedValue]): TaggedValue
proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr): TaggedValue
proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue): TaggedValue
proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue, argument1: TaggedValue): TaggedValue

########################################################################################################################
# Execution frames.                                                                                                    #
########################################################################################################################

# TODO (vm): We should possibly clear a frame by nilling all references so that pointers left in registers aren't
#            causing memory leaks. However, this also incurs a big performance penalty, so we should probably rather
#            verify first that this is a problem before fixing it.
proc create_frame(instance: ptr FunctionInstance, lambda_context: LambdaContext, frame_base: pointer): FramePtr {.inline.} =
  let frame = cast[FramePtr](frame_base)
  frame.function = instance.function
  frame.type_arguments = instance.type_arguments
  frame.lambda_context = lambda_context
  frame

########################################################################################################################
# Global variable functions.                                                                                           #
########################################################################################################################

proc get_global*(variable: GlobalVariable, frame_mem: pointer): TaggedValue =
  ## Gets the global variable's `value`. If the variable hasn't been initialized yet, `get_global` performs the
  ## initialization first and then returns the new value. `frame_mem` must be provided for the execution of the
  ## initialization function.
  ##
  ## Note that `get_global` cannot be placed in the module `definitions` because of a mutual dependency with `evaluate`.
  if not variable.is_initialized:
    variable.value = evaluate(addr variable.initializer, frame_mem)
    variable.is_initialized = true
  variable.value

proc set_global*(variable: GlobalVariable, value: TaggedValue) =
  ## Sets the global variable's `value`, overwriting any previous value. Also sets `is_initialized` to true. This
  ## function also works with lazy global variables.
  variable.is_initialized = true
  variable.value = value

########################################################################################################################
# Register access: General.                                                                                            #
########################################################################################################################

template reg_get(index): untyped = frame.registers[index]
template reg_get_arg(index): untyped = reg_get(instruction.arg(index))

template reg_set(target_index, value): untyped =
  frame.registers[target_index] = value
template reg_set_arg(target_index, value): untyped = reg_set(instruction.arg(target_index), value)

########################################################################################################################
# Register access: Values.                                                                                             #
########################################################################################################################

template regv_get(index): untyped = cast[TaggedValue](reg_get(index))
template regv_get_arg(index): untyped = regv_get(instruction.arg(index))

template regv_get_ref(index, tpe): untyped = untag_reference(regv_get(index), tpe)
template regv_get_int(index): untyped = untag_int(regv_get(index))
template regv_get_bool(index): untyped = untag_boolean(regv_get(index))

template regv_get_ref_arg(index, tpe): untyped = regv_get_ref(instruction.arg(index), tpe)
template regv_get_int_arg(index): untyped = regv_get_int(instruction.arg(index))
template regv_get_bool_arg(index): untyped = regv_get_bool(instruction.arg(index))

template regv_set(target_index, tagged_value): untyped = reg_set(target_index, cast[uint64](tagged_value))
template regv_set_arg(target_index, tagged_value): untyped = regv_set(instruction.arg(target_index), tagged_value)

template regv_set_ref(index, value): untyped = regv_set(index, tag_reference(value))
template regv_set_int(index, value): untyped = regv_set(index, tag_int(value))
template regv_set_bool(index, value): untyped = regv_set(index, tag_boolean(value))

template regv_set_ref_arg(index, value): untyped = regv_set_ref(instruction.arg(index), value)
template regv_set_int_arg(index, value): untyped = regv_set_int(instruction.arg(index), value)
template regv_set_bool_arg(index, value): untyped = regv_set_bool(instruction.arg(index), value)

########################################################################################################################
# Register access: Types.                                                                                              #
########################################################################################################################

template regt_get(index): untyped = cast[Type](reg_get(index))
template regt_get_arg(index): untyped = regt_get(instruction.arg(index))

template regt_set(target_index, tpe): untyped = reg_set(target_index, cast[uint64](tpe))
template regt_set_arg(target_index, tpe): untyped = regt_set(instruction.arg(target_index), tpe)

########################################################################################################################
# Constant table access.                                                                                               #
########################################################################################################################

# TODO (vm): Using `constants` directly becomes awkward when procs are involved. The macros should instead access
#            `frame.function.constants`, but we have to make sure that this doesn't adversely affect performance.

template const_types(index): untyped = constants.types[index]
template const_types_arg(index): untyped = const_types(instruction.arg(index))

template const_value(index): untyped = constants.values[index]
template const_value_arg(index): untyped = const_value(instruction.arg(index))

template const_value_ref(index, tpe): untyped = untag_reference(const_value(index), tpe)
template const_value_ref_arg(index, tpe): untyped = const_value_ref(instruction.arg(index), tpe)

template const_name_arg(index): untyped = constants.names[instruction.arg(index)]
template const_intrinsic_arg(index): untyped = constants.intrinsics[instruction.arg(index)]
template const_schema_arg(index): untyped = constants.schemas[instruction.arg(index)]
template const_global_variable_arg(index): untyped = constants.global_variables[instruction.arg(index)]
template const_multi_function_arg(index): untyped = constants.multi_functions[instruction.arg(index)]
template const_function_instance_arg(index): untyped = constants.function_instances[instruction.arg(index)]
template const_meta_shape_arg(index): untyped = constants.meta_shapes[instruction.arg(1)]

########################################################################################################################
# Operand list support.                                                                                                #
########################################################################################################################

# TODO (vm/parallel): This needs to be unique per thread.
var operand_list: array[operand_list_limit, uint64]

let value_operand_list = cast[ptr UncheckedArray[TaggedValue]](addr operand_list)
let type_operand_list = cast[ptr UncheckedArray[Type]](addr operand_list)

template opl_set_arg(offset: uint16, register_index: uint16): untyped =
  ## The base index is read from `arg0`. `offset` defines the offset from this base index.
  let index = instruction.arg(0) + offset
  operand_list[index] = reg_get_arg(register_index)

template oplv_get_open_array_arg(count_arg_index): untyped =
  to_open_array(value_operand_list, 0, int(instruction.arg(count_arg_index)) - 1)

template oplv_get_open_array_arg_range(count_arg_index, start): untyped =
  to_open_array(value_operand_list, start, start + int(instruction.arg(count_arg_index)) - 1)

template oplv_get_imseq_arg(count_arg_index): untyped =
  new_immutable_seq(value_operand_list, int(instruction.arg(count_arg_index)))

template opl_push_n(count: uint16): untyped =
  for i in 0'u16 ..< count:
    opl_set_arg(i, i + 1)

########################################################################################################################
# Unary, binary, and xary operators.                                                                                   #
########################################################################################################################

macro generate_operand_get(index_node: int, kind: static[string]): untyped =
  case kind
  of "int":
    quote do:
      regv_get_int_arg(`index_node`)
  of "real":
    quote do:
      regv_get_ref_arg(`index_node`, RealValue).real
  of "bool":
    quote do:
      regv_get_bool_arg(`index_node`)
  else: macros.error("Unsupported kind."); nil

macro generate_operation_set_result(index_node: int, result_node: untyped, kind: static[string]): untyped =
  case kind
  of "int":
    quote do:
      regv_set_int_arg(`index_node`, `result_node`)
  of "real":
    quote do:
      regv_set_ref_arg(`index_node`, values.new_real_value(`result_node`))
  of "bool":
    quote do:
      regv_set_bool_arg(`index_node`, `result_node`)
  else: macros.error("Unsupported kind."); nil

macro generate_unary_operator(source_kind: static[string], result_kind: static[string], op_node) =
  ## Generates a unary operator which takes `reg(arg1)` as an operand and puts the result in `reg(arg0)`. The `op_node`
  ## should be defined in terms of an untagged value `v`, e.g. `not v`.
  quote do:
    let v {.inject.} = generate_operand_get(1, `source_kind`)
    generate_operation_set_result(0, `op_node`, `result_kind`)

macro generate_binary_operator(source_kind: static[string], result_kind: static[string], op_node) =
  ## Generates a binary operator which takes `reg(arg1)` and `reg(arg2)` as operands and puts the result in
  ## `reg(arg0)`. The `op_node` should be defined in terms of two untagged values `a` and `b`, e.g. `a + b`.
  quote do:
    let a {.inject.} = generate_operand_get(1, `source_kind`)
    let b {.inject.} = generate_operand_get(2, `source_kind`)
    generate_operation_set_result(0, `op_node`, `result_kind`)

########################################################################################################################
# Operations with direct type and value arguments.                                                                     #
########################################################################################################################

type DirectValueArgumentArray = array[maximum_instruction_arguments, TaggedValue]

template get_direct_type_arguments(argument_count: int, base_index: int): ImSeq[Type] =
  ## Generates a procedure which gets the direct type arguments from the instruction. The argument count is the actual
  ## number of direct arguments. The base index determines the first argument index. Because types are usually expected
  ## in an ImSeq, we're creating an ImSeq right away.
  var type_arguments = new_immutable_seq[Type](argument_count)
  for i in 0 ..< argument_count:
    type_arguments[i] = regt_get_arg(base_index + i)
  type_arguments

template get_direct_value_arguments_open_array(argument_count_node: int, base_index_node: int): untyped =
  ## Generates a procedure which gets the direct value arguments from the instruction. The argument count is the actual
  ## number of direct arguments. The base index determines the first argument index.
  # To simplify this template for different prefix lengths, we set the maximum number of possible direct arguments to
  # `maximum_instruction_arguments`. This will never be reached, but is a safe assumption. If, for example, the prefix
  # length of the instruction is 2 (for example a target register and the argument count), `value_arguments` would have
  # to be at least 5 elements wide.
  let argument_count = argument_count_node
  let base_index = base_index_node
  var value_arguments: DirectValueArgumentArray
  for i in 0 ..< argument_count:
    value_arguments[i] = regv_get_arg(base_index + i)
  to_open_array(value_arguments, 0, argument_count - 1)

########################################################################################################################
# Function calls.                                                                                                      #
########################################################################################################################

# TODO (vm): We can merge the `generate_*` functions into macros.
# TODO (vm): Maybe these should also be inline procs to cut down on too excessive eval-loop size.

# The `function_call` functions are procs to avoid overusing templates. The compiler should decide on its own whether
# to inline these calls.

template next_frame_base(): pointer = cast[pointer](cast[uint](frame) + frame.function.frame_size)

template call_start(target: ptr FunctionInstance, lambda_context: LambdaContext): FramePtr =
  let target_frame_base = next_frame_base()
  create_frame(target, lambda_context, target_frame_base)

template get_call_result(target_frame): untyped =
  # After function evaluation has finished, it must guarantee that the return value is in the first register.
  cast[TaggedValue](target_frame.registers[0])

template generate_call(target: ptr FunctionInstance, lambda_context: LambdaContext, arguments): TaggedValue =
  let target_frame = call_start(target, lambda_context)
  for i in 0 ..< arguments.len:
    target_frame.registers[i] = cast[uint64](arguments[i])
  evaluate(target_frame)
  get_call_result(target_frame)

# TODO (vm): Do we have to pass type arguments of an owning multi-function to a lambda? Or how does this work?
template generate_call0(target: ptr FunctionInstance, lambda_context: LambdaContext): TaggedValue =
  let target_frame = call_start(target, lambda_context)
  evaluate(target_frame)
  get_call_result(target_frame)

template generate_call1(target: ptr FunctionInstance, lambda_context: LambdaContext, argument0): TaggedValue =
  let target_frame = call_start(target, lambda_context)
  target_frame.registers[0] = cast[uint64](argument0)
  evaluate(target_frame)
  get_call_result(target_frame)

template generate_call2(target: ptr FunctionInstance, lambda_context: LambdaContext, argument0, argument1): TaggedValue =
  let target_frame = call_start(target, lambda_context)
  target_frame.registers[0] = cast[uint64](argument0)
  target_frame.registers[1] = cast[uint64](argument1)
  evaluate(target_frame)
  get_call_result(target_frame)

template generate_dispatch(mf, arguments): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target_from_arguments(mf, arguments, target)
  generate_call(addr target, LambdaContext(nil), arguments)

template generate_dispatch0(mf): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target_from_arguments(mf, target)
  generate_call0(addr target, LambdaContext(nil))

template generate_dispatch1(mf, argument0): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target_from_arguments(mf, argument0, target)
  generate_call1(addr target, LambdaContext(nil), argument0)

template generate_dispatch2(mf, argument0, argument1): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target_from_arguments(mf, argument0, argument1, target)
  generate_call2(addr target, LambdaContext(nil), argument0, argument1)

########################################################################################################################
# Function values.                                                                                                     #
########################################################################################################################

proc lambda_get_function_instance(frame: FramePtr, instruction: Instruction, arg_index: uint16): ptr FunctionInstance {.inline.} =
  let constants = frame.function.constants
  let mf = const_multi_function_arg(arg_index)

  # We don't have to check the bounds of the type parameters, because a lambda function's must be boundless.
  mf.instantiate_single_function_unchecked(frame.type_arguments)

template generate_lambda(is_poly: bool) =
  let function_instance = lambda_get_function_instance(frame, instruction, 1)
  let tpe = if is_poly: substitute(const_types_arg(2), frame.type_arguments) else: const_types_arg(2)
  let context = LambdaContext(oplv_get_imseq_arg(3))
  let value = values.new_single_function_value(function_instance, context, tpe)
  regv_set_ref_arg(0, value)

template generate_lambda0(is_poly: bool) =
  let function_instance = lambda_get_function_instance(frame, instruction, 1)
  let tpe = if is_poly: substitute(const_types_arg(2), frame.type_arguments) else: const_types_arg(2)
  let value = values.new_single_function_value(function_instance, LambdaContext(nil), tpe)
  regv_set_ref_arg(0, value)

########################################################################################################################
# Lists.                                                                                                               #
########################################################################################################################

template generate_list_append(new_tpe): untyped =
  let list = regv_get_ref_arg(1, ListValue)
  let new_element = regv_get_arg(2)
  var new_elements = list.elements.append(new_element)
  regv_set_ref_arg(0, new_list_value(new_elements, new_tpe))

########################################################################################################################
# Intrinsics.                                                                                                          #
########################################################################################################################

# TODO (vm): Maybe intrinsic calls should be inline procs to cut down on too excessive eval-loop size.

macro generate_intrisic_evaluation(
  is_void: static[bool],
  is_frame_aware: static[bool],
  argument_count: static[int],
) =
  ## Generates an evaluation of an intrinsic call based on the given parameters. The three options are essentially
  ## orthogonal, which means that to implement intrinsic evaluation for voidness, frame awareness, and `0, 1, 2, n`
  ## arguments, we'd need 4 * 2 * 2 = 16 implementations. Hence, the macro seems preferable.
  ##
  ## The macro generates code such as this (is_void = false, frame-aware = true, arguments = 2):
  ##
  ##    let intrinsic = const_intrinsic_arg(1)
  ##    let function = intrinsic.function.binary_fa
  ##    let argument0 = regv_get_arg(2)
  ##    let argument1 = regv_get_arg(3)
  ##    regv_set_arg(0, function(frame, argument0, argument1))
  var xary_name =
    case argument_count
    of 0: "nullary"
    of 1: "unary"
    of 2: "binary"
    else:
      macros.error("An intrinsic argument count greater than 2 is not supported yet.")
      ""
  if is_frame_aware:
    xary_name = xary_name & "_fa"

  let xary_id = macros.ident(xary_name)
  let intrinsic_id = macros.ident("intrinsic")
  let function_id = macros.ident("function")

  let intrinsic_arg_index = macros.new_lit(if not is_void: 1 else: 0)
  var statements: seq[NimNode] = @[
    quote do:
      let `intrinsic_id` = const_intrinsic_arg(`intrinsic_arg_index`)
    ,
    quote do:
      let `function_id` = `intrinsic_id`.function.`xary_id`
  ]

  var function_arguments: seq[NimNode] = @[]
  if is_frame_aware:
    function_arguments.add(macros.ident("frame"))

  let argument_offset = if not is_void: 2 else: 1
  for i in 0 ..< argument_count:
    let argument_id = macros.ident("argument" & $i)
    let arg_index = macros.new_lit(i + argument_offset)
    statements.add(
      quote do:
        let `argument_id` = regv_get_arg(`arg_index`)
    )
    function_arguments.add(argument_id)

  let function_call = macros.new_call(function_id, function_arguments)
  if is_void:
    statements.add(
      quote do:
        discard `function_call`
    )
  else:
    statements.add(
      quote do:
        let res = `function_call`
        regv_set_arg(0, res)
    )

  macros.new_stmt_list(statements)

########################################################################################################################
# Evaluate.                                                                                                            #
########################################################################################################################

proc evaluate(frame: FramePtr) =
  when_debug: echo "Evaluating function ", frame.function.multi_function.name, " with frame ", cast[uint](frame)

  let instructions = frame.function.instructions
  let constants = frame.function.constants

  var pc: uint16 = 0

  while true:
    {.computedgoto.}
    let instruction = instructions[pc]
    when_debug: echo instruction.operation, " (PC: ", pc, ")"
    pc += 1

    case instruction.operation
    of Operation.Assign:
      reg_set_arg(0, reg_get_arg(1))

    of Operation.Const:
      regv_set_arg(0, const_value_arg(1))

    of Operation.IntConst:
      regv_set_int_arg(0, instruction.argi(1))

    of Operation.IntNeg: generate_unary_operator("int", "int", -v)
    of Operation.IntAdd: generate_binary_operator("int", "int", a + b)
    of Operation.IntSub: generate_binary_operator("int", "int", a - b)
    of Operation.IntMul: generate_binary_operator("int", "int", a * b)
    of Operation.IntDiv: generate_binary_operator("int", "int", a div b)
    of Operation.IntEq: generate_binary_operator("int", "bool", a == b)
    of Operation.IntLt: generate_binary_operator("int", "bool", a < b)
    of Operation.IntLte: generate_binary_operator("int", "bool", a <= b)
    of Operation.IntToReal: generate_unary_operator("int", "real", float64(v))

    of Operation.RealNeg: generate_unary_operator("real", "real", -v)
    of Operation.RealAdd: generate_binary_operator("real", "real", a + b)
    of Operation.RealSub: generate_binary_operator("real", "real", a - b)
    of Operation.RealMul: generate_binary_operator("real", "real", a * b)
    of Operation.RealDiv: generate_binary_operator("real", "real", a / b)
    of Operation.RealEq: generate_binary_operator("real", "bool", a == b)
    of Operation.RealLt: generate_binary_operator("real", "bool", a < b)
    of Operation.RealLte: generate_binary_operator("real", "bool", a <= b)

    of Operation.BooleanConst:
      regv_set_bool_arg(0, instruction.argb(1))

    of Operation.BooleanNot: generate_unary_operator("bool", "bool", not v)
    of Operation.BooleanOr: generate_binary_operator("bool", "bool", a or b)
    of Operation.BooleanAnd: generate_binary_operator("bool", "bool", a and b)

    of Operation.StringOf:
      let string = $regv_get_arg(1)
      regv_set_ref_arg(0, values.new_string_value(string))

    of Operation.StringConcat:
      let a = regv_get_ref_arg(1, StringValue)
      let b = regv_get_ref_arg(2, StringValue)
      regv_set_ref_arg(0, values.new_string_value(a.string & b.string))

    of Operation.StringEq:
      let a = regv_get_ref_arg(1, StringValue)
      let b = regv_get_ref_arg(2, StringValue)
      regv_set_bool_arg(0, a.string == b.string)

    of Operation.StringLt: quit("StringLt is not yet implemented.")
    of Operation.StringLte: quit("StringLte is not yet implemented.")

    of Operation.Tuple:
      var elements = oplv_get_imseq_arg(1)
      regv_set_ref_arg(0, values.new_tuple_value(elements))

    # TODO (vm): Implement TupleX with a single macro.
    of Operation.Tuple0:
      regv_set_arg(0, values.unit_value_tagged)

    of Operation.Tuple1:
      var elements = new_immutable_seq[TaggedValue](1)
      elements[0] = regv_get_arg(1)
      regv_set_ref_arg(0, values.new_tuple_value(elements))

    of Operation.Tuple2:
      var elements = new_immutable_seq[TaggedValue](2)
      elements[0] = regv_get_arg(1)
      elements[1] = regv_get_arg(2)
      regv_set_ref_arg(0, values.new_tuple_value(elements))

    of Operation.TupleGet:
      let tpl = regv_get_ref_arg(1, TupleValue)
      regv_set_arg(0, tpl.elements[instruction.arg(2)])

    of Operation.FunctionCall:
      let function = regv_get_ref_arg(1, FunctionValue)
      let res = evaluate_function_value(function, frame, oplv_get_open_array_arg(2))
      regv_set_arg(0, res)

    # TODO (vm): Implement FunctionCallX with a single macro.
    of Operation.FunctionCall0:
      let function = regv_get_ref_arg(1, FunctionValue)
      let res = evaluate_function_value(function, frame)
      regv_set_arg(0, res)

    of Operation.FunctionCall1:
      let function = regv_get_ref_arg(1, FunctionValue)
      let argument0 = regv_get_arg(2)
      let res = evaluate_function_value(function, frame, argument0)
      regv_set_arg(0, res)

    of Operation.FunctionCall2:
      let function = regv_get_ref_arg(1, FunctionValue)
      let argument0 = regv_get_arg(2)
      let argument1 = regv_get_arg(3)
      let res = evaluate_function_value(function, frame, argument0, argument1)
      regv_set_arg(0, res)

    of Operation.Lambda: generate_lambda(false)
    of Operation.Lambda0: generate_lambda0(false)
    of Operation.LambdaPoly: generate_lambda(true)
    of Operation.LambdaPoly0: generate_lambda0(true)

    of Operation.LambdaLocal:
      if ImSeq[TaggedValue](frame.lambda_context) == nil:
        quit(fmt"`LambdaLocal` cannot be executed because the lambda context is `nil`.")
      regv_set_arg(0, frame.lambda_context[instruction.arg(1)])

    of Operation.List:
      let elements = oplv_get_imseq_arg(2)
      let list = values.new_list_value(elements, const_types_arg(1))
      regv_set_ref_arg(0, list)

    of Operation.List0:
      let list = values.new_list_value(empty_immutable_seq[TaggedValue](), const_types_arg(1))
      regv_set_ref_arg(0, list)

    of Operation.List1:
      let list = values.new_list_value(new_immutable_seq([regv_get_arg(2)]), const_types_arg(1))
      regv_set_ref_arg(0, list)

    of Operation.ListPoly:
      let tpe = substitute(const_types_arg(1), frame.type_arguments)
      let elements = oplv_get_imseq_arg(2)
      let list = values.new_list_value(elements, tpe)
      regv_set_ref_arg(0, list)

    of Operation.ListPoly0:
      let tpe = substitute(const_types_arg(1), frame.type_arguments)
      let list = values.new_list_value(empty_immutable_seq[TaggedValue](), tpe)
      regv_set_ref_arg(0, list)

    of Operation.ListPoly1:
      let tpe = substitute(const_types_arg(1), frame.type_arguments)
      let list = values.new_list_value(new_immutable_seq([regv_get_arg(2)]), tpe)
      regv_set_ref_arg(0, list)

    of Operation.ListAppend:
      let new_tpe = const_types_arg(3)
      generate_list_append(new_tpe)

    of Operation.ListAppendPoly:
      let new_tpe = substitute(const_types_arg(3), frame.type_arguments)
      generate_list_append(new_tpe)

    of Operation.ListAppendUntyped:
      let list = regv_get_ref_arg(1, ListValue)
      generate_list_append(list.tpe)

    of Operation.ListLength:
      let list = regv_get_ref_arg(1, ListValue)
      regv_set_int_arg(0, list.elements.len)

    of Operation.ListGet:
      let list = regv_get_ref_arg(1, ListValue)
      let index = regv_get_int_arg(2)
      regv_set_arg(0, list.elements[index])

    of Operation.Shape:
      let meta_shape = const_meta_shape_arg(1)
      regv_set_ref_arg(0, values.new_shape_value(meta_shape, oplv_get_open_array_arg(2)))

    of Operation.Shape0:
      regv_set_arg(0, values.empty_shape)

    # TODO (vm): Implement ShapeX with a single macro.
    of Operation.Shape1:
      let meta_shape = const_meta_shape_arg(1)
      regv_set_ref_arg(0, values.new_shape_value(meta_shape, [regv_get_arg(2)]))

    of Operation.Shape2:
      let meta_shape = const_meta_shape_arg(1)
      regv_set_ref_arg(0, values.new_shape_value(meta_shape, [regv_get_arg(2), regv_get_arg(3)]))

    of Operation.ShapePropertyGetNamed:
      let shape = regv_get_ref_arg(1, ShapeValue)
      let name = const_name_arg(2)
      regv_set_arg(0, shape.get_property_value(name))

    of Operation.SymbolEq:
      let a = regv_get_ref_arg(1, SymbolValue)
      let b = regv_get_ref_arg(2, SymbolValue)
      regv_set_bool_arg(0, a.name == b.name)

    of Operation.Struct:
      let tpe = cast[StructType](const_types_arg(1))
      let value = new_struct_value(tpe, oplv_get_open_array_arg(2))
      regv_set_ref_arg(0, value)

    of Operation.StructDirect:
      let tpe = cast[StructType](const_types_arg(1))
      let nv = int(instruction.arg(2))
      let value = new_struct_value(tpe, get_direct_value_arguments_open_array(nv, 3))
      regv_set_ref_arg(0, value)

    of Operation.StructPoly:
      let schema = cast[StructSchema](const_schema_arg(1))
      let nt = int(instruction.arg(2))
      let value = values.new_struct_value(
        schema,
        new_immutable_seq(type_operand_list, nt),
        oplv_get_open_array_arg_range(3, nt),
      )
      regv_set_ref_arg(0, value)

    of Operation.StructPolyDirect:
      let schema = cast[StructSchema](const_schema_arg(1))
      let nt = int(instruction.argu8l(2))
      let nv = int(instruction.argu8r(2))
      let value = values.new_struct_value(
        schema,
        get_direct_type_arguments(nt, 3),
        get_direct_value_arguments_open_array(nv, 3 + nt),
      )
      regv_set_ref_arg(0, value)

    of Operation.StructPropertyGet:
      let struct = regv_get_ref_arg(1, StructValue)
      regv_set_arg(0, struct.property_values[instruction.arg(2)])

    of Operation.StructPropertyGetNamed:
      let struct = regv_get_ref_arg(1, StructValue)
      let name = const_name_arg(2)
      regv_set_arg(0, struct.get_property_value(name))

    of Operation.StructEq:
      let a = regv_get_ref_arg(1, StructValue)
      let b = regv_get_ref_arg(2, StructValue)
      regv_set_bool_arg(0, cast[pointer](a) == cast[pointer](b))

    of Operation.PropertyGetNamed:
      let instance = regv_get_arg(1)
      let name = const_name_arg(2)
      let value = instance.get_property_value(name)
      regv_set_arg(0, value)

    of Operation.Jump:
      pc = instruction.arg(0)

    of Operation.JumpIfFalse:
      let predicate = regv_get_bool_arg(1)
      if (not predicate):
        pc = instruction.arg(0)

    of Operation.JumpIfTrue:
      let predicate = regv_get_bool_arg(1)
      if (predicate):
        pc = instruction.arg(0)

    of Operation.Intrinsic0: generate_intrisic_evaluation(false, false, 0)
    of Operation.Intrinsic1: generate_intrisic_evaluation(false, false, 1)
    of Operation.Intrinsic2: generate_intrisic_evaluation(false, false, 2)
    of Operation.IntrinsicVoid0: generate_intrisic_evaluation(true, false, 0)
    of Operation.IntrinsicVoid1: generate_intrisic_evaluation(true, false, 1)
    of Operation.IntrinsicVoid2: generate_intrisic_evaluation(true, false, 2)
    of Operation.IntrinsicFa1: generate_intrisic_evaluation(false, true, 1)
    of Operation.IntrinsicFa2: generate_intrisic_evaluation(false, true, 2)
    of Operation.IntrinsicVoidFa1: generate_intrisic_evaluation(true, true, 1)
    of Operation.IntrinsicVoidFa2: generate_intrisic_evaluation(true, true, 2)

    of Operation.GlobalGetEager:
      let gv = const_global_variable_arg(1)
      regv_set_arg(0, gv.value)

    of Operation.GlobalGetLazy:
      let gv = const_global_variable_arg(1)
      let frame_base = next_frame_base()
      let value = get_global(gv, frame_base)
      regv_set_arg(0, value)

    of Operation.GlobalSet:
      let gv = const_global_variable_arg(0)
      let value = regv_get_arg(1)
      set_global(gv, value)

    of Operation.Dispatch:
      let mf = const_multi_function_arg(1)
      let res = generate_dispatch(mf, oplv_get_open_array_arg(2))
      regv_set_arg(0, res)

    # TODO (vm): Implement DispatchX with a single macro.
    of Operation.Dispatch0:
      let mf = const_multi_function_arg(1)
      let res = generate_dispatch0(mf)
      regv_set_arg(0, res)

    of Operation.Dispatch1:
      let mf = const_multi_function_arg(1)
      let argument0 = regv_get_arg(2)
      let res = generate_dispatch1(mf, argument0)
      regv_set_arg(0, res)

    of Operation.Dispatch2:
      let mf = const_multi_function_arg(1)
      let argument0 = regv_get_arg(2)
      let argument1 = regv_get_arg(3)
      let res = generate_dispatch2(mf, argument0, argument1)
      regv_set_arg(0, res)

    of Operation.Call:
      let function_instance = const_function_instance_arg(1)
      let value = generate_call(function_instance, LambdaContext(nil), oplv_get_open_array_arg(2))
      regv_set_arg(0, value)

    of Operation.CallDirect:
      let function_instance = const_function_instance_arg(1)
      let nv = int(instruction.arg(2))
      let value = generate_call(function_instance, LambdaContext(nil), get_direct_value_arguments_open_array(nv, 3))
      regv_set_arg(0, value)

    of Operation.CallPoly:
      let mf = const_multi_function_arg(1)
      let nt = int(instruction.arg(2))
      let function_instance = mf.instantiate_single_function(new_immutable_seq(type_operand_list, nt))
      let value = generate_call(function_instance, LambdaContext(nil), oplv_get_open_array_arg_range(3, nt))
      regv_set_arg(0, value)

    of Operation.CallPolyDirect:
      let mf = const_multi_function_arg(1)
      let nt = int(instruction.argu8l(2))
      let nv = int(instruction.argu8r(2))
      let function_instance = mf.instantiate_single_function(get_direct_type_arguments(nt, 3))
      let value = generate_call(function_instance, LambdaContext(nil), get_direct_value_arguments_open_array(nv, 3 + nt))
      regv_set_arg(0, value)

    of Operation.Return:
      regv_set(0, regv_get_arg(0))
      break

    of Operation.Return0:
      break

    of Operation.TypeArg:
      regt_set_arg(0, frame.type_arguments[instruction.arg(1)])

    of Operation.TypeConst:
      regt_set_arg(0, const_types_arg(1))

    of Operation.OplPush1: opl_push_n(1)
    of Operation.OplPush2: opl_push_n(2)
    of Operation.OplPush3: opl_push_n(3)
    of Operation.OplPush4: opl_push_n(4)
    of Operation.OplPush5: opl_push_n(5)
    of Operation.OplPush6: opl_push_n(6)

    of Operation.Invalid:
      quit("The VM encountered an `Invalid` operation. This was likely caused by faulty bytecode or bytecode resolution.")

proc evaluate*(entry_function: ptr FunctionInstance, frame_mem: pointer): TaggedValue =
  let frame = create_frame(entry_function, LambdaContext(nil), frame_mem)
  evaluate(frame)

  # The bytecode must ensure that the result is in the first register.
  regv_get(0)

# TODO (vm): `evaluate_function_value` can be implemented with a macro.

proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, arguments: open_array[TaggedValue]): TaggedValue =
  ## Evaluates a function value with a signature `(...) => Any`.
  assert(arity(function_value) == arguments.len)

  var function_instance = FunctionInstance()
  let target =
    if function_value.variant == FunctionValueVariant.Multi:
      find_dispatch_target_from_arguments(cast[MultiFunction](function_value.target), arguments, function_instance)
      addr function_instance
    else:
      cast[ptr FunctionInstance](function_value.target)
  generate_call(target, function_value.context, arguments)

proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr): TaggedValue =
  ## Evaluates a function value with a signature `() => Any`.
  assert(arity(function_value) == 0)

  var function_instance = FunctionInstance()
  let target =
    if function_value.variant == FunctionValueVariant.Multi:
      find_dispatch_target_from_arguments(cast[MultiFunction](function_value.target), function_instance)
      addr function_instance
    else:
      cast[ptr FunctionInstance](function_value.target)
  generate_call0(target, function_value.context)

proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue): TaggedValue =
  ## Evaluates a function value with a signature `(Any) => Any`.
  assert(arity(function_value) == 1)

  var function_instance = FunctionInstance()
  let target =
    if function_value.variant == FunctionValueVariant.Multi:
      find_dispatch_target_from_arguments(cast[MultiFunction](function_value.target), argument0, function_instance)
      addr function_instance
    else:
      cast[ptr FunctionInstance](function_value.target)
  generate_call1(target, function_value.context, argument0)

proc evaluate_function_value*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue, argument1: TaggedValue): TaggedValue =
  ## Evaluates a function value with a signature `(Any, Any) => Any`.
  assert(arity(function_value) == 2)

  var function_instance = FunctionInstance()
  let target =
    if function_value.variant == FunctionValueVariant.Multi:
      find_dispatch_target_from_arguments(cast[MultiFunction](function_value.target), argument0, argument1, function_instance)
      addr function_instance
    else:
      cast[ptr FunctionInstance](function_value.target)
  generate_call2(target, function_value.context, argument0, argument1)
