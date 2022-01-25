import std/macros

import definitions
from dispatch import find_dispatch_target
import imseqs
import instructions
from types import nil
import values
from utils import when_debug

proc evaluate*(entry_function: ptr FunctionInstance, frame_mem: pointer): TaggedValue

proc evaluate*(function_value: FunctionValue, frame: FramePtr): TaggedValue
proc evaluate*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue): TaggedValue
proc evaluate*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue, argument1: TaggedValue): TaggedValue

########################################################################################################################
# Execution frames.                                                                                                    #
########################################################################################################################

# TODO (vm): We should possibly clear a frame by nilling all references so that pointers left in registers aren't
#            causing memory leaks. However, this also incurs a big performance penalty, so we should probably rather
#            verify first that this is a problem before fixing it.
proc create_frame(instance: ptr FunctionInstance, frame_base: pointer): FramePtr {.inline.} =
  let frame = cast[FramePtr](frame_base)
  frame.function = instance.function
  frame.type_arguments = instance.type_arguments
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
# Evaluator implementation.                                                                                            #
########################################################################################################################

proc evaluate(frame: FramePtr)

template reg_get(index): untyped = frame.registers[index]
template reg_get_arg(index): untyped = reg_get(instruction.arg(index))

template reg_get_ref(index, tpe): untyped = untag_reference(reg_get(index), tpe)
template reg_get_int(index): untyped = untag_int(reg_get(index))
template reg_get_bool(index): untyped = untag_boolean(reg_get(index))

template reg_get_ref_arg(index, tpe): untyped = reg_get_ref(instruction.arg(index), tpe)
template reg_get_int_arg(index): untyped = reg_get_int(instruction.arg(index))
template reg_get_bool_arg(index): untyped = reg_get_bool(instruction.arg(index))

template reg_set(target_index, register_value): untyped =
  frame.registers[target_index] = register_value

template reg_set_arg(index, register_value): untyped = reg_set(instruction.arg(index), register_value)

template reg_set_ref(index, value): untyped = reg_set(index, tag_reference(value))
template reg_set_int(index, value): untyped = reg_set(index, tag_int(value))
template reg_set_bool(index, value): untyped = reg_set(index, tag_boolean(value))

template reg_set_ref_arg(index, value): untyped = reg_set_ref(instruction.arg(index), value)
template reg_set_int_arg(index, value): untyped = reg_set_int(instruction.arg(index), value)
template reg_set_bool_arg(index, value): untyped = reg_set_bool(instruction.arg(index), value)

template const_types(index): untyped = constants.types[index]
template const_types_arg(index): untyped = const_types(instruction.arg(index))

template const_value(index): untyped = constants.values[index]
template const_value_arg(index): untyped = const_value(instruction.arg(index))

template const_value_ref(index, tpe): untyped = untag_reference(const_value(index), tpe)
template const_value_ref_arg(index, tpe): untyped = const_value_ref(instruction.arg(index), tpe)

template const_name_arg(index): untyped = constants.names[instruction.arg(index)]
template const_intrinsic_arg(index): untyped = constants.intrinsics[instruction.arg(index)]
template const_global_variable_arg(index): untyped = constants.global_variables[instruction.arg(index)]
template const_multi_function_arg(index): untyped = constants.multi_functions[instruction.arg(index)]
template const_meta_shape_arg(index): untyped = constants.meta_shapes[instruction.arg(1)]

template list_append(new_tpe): untyped =
  let list = reg_get_ref_arg(1, ListValue)
  let new_element = reg_get_arg(2)
  var new_elements = list.elements.append(new_element)
  reg_set_ref_arg(0, new_list(new_elements, new_tpe))

template next_frame_base(): pointer = cast[pointer](cast[uint](frame) + frame.function.frame_size)

template call_start(target: ptr FunctionInstance): FramePtr =
  let target_frame_base = next_frame_base()
  create_frame(target, target_frame_base)

template get_call_result(target_frame): untyped =
  # After function evaluation has finished, it must guarantee that the return value is in the first register.
  target_frame.registers[0]

# TODO (vm): Do we have to pass type arguments of an owning multi-function to a lambda? Or how does this work?
template call0(target: ptr FunctionInstance): TaggedValue =
  let target_frame = call_start(target)
  evaluate(target_frame)
  get_call_result(target_frame)

template call1(target: ptr FunctionInstance, argument0): TaggedValue =
  let target_frame = call_start(target)
  target_frame.registers[0] = argument0
  evaluate(target_frame)
  get_call_result(target_frame)

template call2(target: ptr FunctionInstance, argument0, argument1): TaggedValue =
  let target_frame = call_start(target)
  target_frame.registers[0] = argument0
  target_frame.registers[1] = argument1
  evaluate(target_frame)
  get_call_result(target_frame)

template dispatch0(mf): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target(mf, target)
  call0(addr target)

template dispatch1(mf, argument0): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target(mf, argument0, target)
  call1(addr target, argument0)

template dispatch2(mf, argument0, argument1): TaggedValue =
  var target = FunctionInstance()
  find_dispatch_target(mf, argument0, argument1, target)
  call2(addr target, argument0, argument1)

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
  ##    let argument0 = reg_get_arg(2)
  ##    let argument1 = reg_get_arg(3)
  ##    reg_set_arg(0, function(frame, argument0, argument1))
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
        let `argument_id` = reg_get_arg(`arg_index`)
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
        reg_set_arg(0, res)
    )

  macros.new_stmt_list(statements)

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
    of Operation.Const:
      reg_set_arg(0, const_value_arg(1))

    of Operation.ConstPoly:
      reg_set_arg(0, substitute_types(const_value_arg(1), frame.type_arguments))

    of Operation.IntConst:
      reg_set_int_arg(0, instruction.argi(1))

    of Operation.IntAdd:
      let a = reg_get_int_arg(1)
      let b = reg_get_int_arg(2)
      reg_set_int_arg(0, a + b)

    of Operation.IntAddConst:
      let a = reg_get_int_arg(1)
      let b = instruction.argi(2)
      reg_set_int_arg(0, a + b)

    of Operation.IntSubConst:
      let a = reg_get_int_arg(1)
      let b = instruction.argi(2)
      reg_set_int_arg(0, a - b)

    of Operation.IntLt:
      let a = reg_get_int_arg(1)
      let b = reg_get_int_arg(2)
      reg_set_bool_arg(0, a < b)

    of Operation.IntLtConst:
      let a = reg_get_int_arg(1)
      let b = instruction.argi(2)
      reg_set_bool_arg(0, a < b)

    of Operation.IntGtConst:
      let a = reg_get_int_arg(1)
      let b = instruction.argi(2)
      reg_set_bool_arg(0, a > b)

    of Operation.RealAdd:
      let a = reg_get_ref_arg(1, RealValue)
      let b = reg_get_ref_arg(2, RealValue)
      reg_set_ref_arg(0, values.new_real(a.real + b.real))

    of Operation.StringOf:
      let string = $reg_get_arg(1)
      reg_set_ref_arg(0, values.new_string(string))

    of Operation.StringConcat:
      let a = reg_get_ref_arg(1, StringValue)
      let b = reg_get_ref_arg(2, StringValue)
      reg_set_ref_arg(0, values.new_string(a.string & b.string))

    of Operation.StringConcatConst:
      let a = reg_get_ref_arg(1, StringValue)
      let b = const_value_ref_arg(2, StringValue)
      reg_set_ref_arg(0, values.new_string(a.string & b.string))

    of Operation.StringConcatConstl:
      let a = const_value_ref_arg(1, StringValue)
      let b = reg_get_ref_arg(2, StringValue)
      reg_set_ref_arg(0, values.new_string(a.string & b.string))

    of Operation.Tuple:
      let first = instruction.arg(1)
      let last = instruction.arg(2)
      var elements = new_immutable_seq[TaggedValue](last - first + 1)
      for i in first .. last:
        elements[i] = reg_get(i)
      reg_set_ref_arg(0, values.new_tuple(elements))

    of Operation.Tuple2:
      var elements = new_immutable_seq[TaggedValue](2)
      elements[0] = reg_get_arg(1)
      elements[1] = reg_get_arg(2)
      reg_set_ref_arg(0, values.new_tuple(elements))

    of Operation.TupleGet:
      let tpl = reg_get_ref_arg(1, TupleValue)
      reg_set_arg(0, tpl.elements[instruction.arg(2)])

    of Operation.FunctionCall0:
      let function = reg_get_ref_arg(1, FunctionValue)
      let res = evaluate(function, frame)
      reg_set_arg(0, res)

    of Operation.FunctionCall1:
      let function = reg_get_ref_arg(1, FunctionValue)
      let argument0 = reg_get_arg(2)
      let res = evaluate(function, frame, argument0)
      reg_set_arg(0, res)

    of Operation.FunctionCall2:
      let function = reg_get_ref_arg(1, FunctionValue)
      let argument0 = reg_get_arg(2)
      let argument1 = reg_get_arg(3)
      let res = evaluate(function, frame, argument0, argument1)
      reg_set_arg(0, res)

    of Operation.ListAppend:
      let new_tpe = const_types_arg(3)
      list_append(new_tpe)

    of Operation.ListAppendPoly:
      let new_tpe = types.substitute(const_types_arg(3), frame.type_arguments)
      list_append(new_tpe)

    of Operation.ListAppendUntyped:
      let list = reg_get_ref_arg(1, ListValue)
      list_append(list.tpe)

    of Operation.Shape:
      let meta_shape = const_meta_shape_arg(1)
      let first = instruction.arg(2)
      let last = instruction.arg(3)

      # We can just pass the register array with the correct first and last indices for shape creation. There is no
      # need to allocate an intermediate sequence.
      reg_set_ref_arg(0, values.new_shape_value(meta_shape, to_open_array(addr frame.registers, int(first), int(last))))

    of Operation.Shape1:
      let meta_shape = const_meta_shape_arg(1)
      reg_set_ref_arg(0, values.new_shape_value(meta_shape, [reg_get_arg(2)]))

    of Operation.Shape2:
      let meta_shape = const_meta_shape_arg(1)
      reg_set_ref_arg(0, values.new_shape_value(meta_shape, [reg_get_arg(2), reg_get_arg(3)]))

    of Operation.ShapeGetProperty:
      let shape = reg_get_ref_arg(1, ShapeValue)
      let name = const_name_arg(2)
      reg_set_arg(0, shape.get_property_value(name))

    of Operation.SymbolEq:
      let a = reg_get_ref_arg(1, SymbolValue)
      let b = reg_get_ref_arg(2, SymbolValue)
      reg_set_bool_arg(0, a.name == b.name)

    of Operation.SymbolEqConst:
      let a = reg_get_ref_arg(1, SymbolValue)
      let b = const_value_ref_arg(2, SymbolValue)
      reg_set_bool_arg(0, a.name == b.name)

    of Operation.Jump:
      pc = instruction.arg(0)

    of Operation.JumpIfFalse:
      let predicate = reg_get_bool_arg(1)
      if (not predicate):
        pc = instruction.arg(0)

    of Operation.JumpIfTrue:
      let predicate = reg_get_bool_arg(1)
      if (predicate):
        pc = instruction.arg(0)

    of Operation.Intrinsic0:
      generate_intrisic_evaluation(false, false, 0)

    of Operation.IntrinsicVoid0:
      generate_intrisic_evaluation(true, false, 0)

    of Operation.Intrinsic1:
      generate_intrisic_evaluation(false, false, 1)

    of Operation.IntrinsicFa1:
      generate_intrisic_evaluation(false, true, 1)

    of Operation.IntrinsicVoid1:
      generate_intrisic_evaluation(true, false, 1)

    of Operation.Intrinsic2:
      generate_intrisic_evaluation(false, false, 2)

    of Operation.IntrinsicFa2:
      generate_intrisic_evaluation(false, true, 2)

    of Operation.IntrinsicVoidFa2:
      generate_intrisic_evaluation(true, true, 2)

    of Operation.GlobalGetEager:
      let gv = const_global_variable_arg(1)
      reg_set_arg(0, gv.value)

    of Operation.GlobalGetLazy:
      let gv = const_global_variable_arg(1)
      let frame_base = next_frame_base()
      let value = get_global(gv, frame_base)
      reg_set_arg(0, value)

    of Operation.GlobalSet:
      let gv = const_global_variable_arg(0)
      let value = reg_get_arg(1)
      set_global(gv, value)

    of Operation.Dispatch1:
      let mf = const_multi_function_arg(1)
      let argument0 = reg_get_arg(2)
      let res = dispatch1(mf, argument0)
      reg_set_arg(0, res)

    of Operation.Dispatch2:
      let mf = const_multi_function_arg(1)
      let argument0 = reg_get_arg(2)
      let argument1 = reg_get_arg(3)
      let res = dispatch2(mf, argument0, argument1)
      reg_set_arg(0, res)

    of Operation.Return:
      reg_set(0, reg_get_arg(0))
      break

    of Operation.ReturnUnit:
      reg_set(0, unit)
      break

    of Operation.Return0:
      break

proc evaluate*(entry_function: ptr FunctionInstance, frame_mem: pointer): TaggedValue =
  let frame = create_frame(entry_function, frame_mem)
  evaluate(frame)

  # The bytecode must ensure that the result is in the first register.
  reg_get(0)

proc evaluate*(function_value: FunctionValue, frame: FramePtr): TaggedValue =
  ## Evaluates a function value with a signature `() => Any`.
  assert(arity(function_value) == 0)
  if function_value.is_fixed:
    call0(cast[ptr FunctionInstance](function_value.target))
  else:
    dispatch0(cast[MultiFunction](function_value.target))

proc evaluate*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue): TaggedValue =
  ## Evaluates a function value with a signature `(Any) => Any`.
  assert(arity(function_value) == 1)
  if function_value.is_fixed:
    call1(cast[ptr FunctionInstance](function_value.target), argument0)
  else:
    dispatch1(cast[MultiFunction](function_value.target), argument0)

proc evaluate*(function_value: FunctionValue, frame: FramePtr, argument0: TaggedValue, argument1: TaggedValue): TaggedValue =
  ## Evaluates a function value with a signature `(Any, Any) => Any`.
  assert(arity(function_value) == 2)
  if function_value.is_fixed:
    call2(cast[ptr FunctionInstance](function_value.target), argument0, argument1)
  else:
    dispatch2(cast[MultiFunction](function_value.target), argument0, argument1)
