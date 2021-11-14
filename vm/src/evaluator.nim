from definitions import GlobalVariable, MultiFunction, Function
from dispatch import get_dispatch_target
import instructions
import values
from utils import when_debug

proc evaluate*(entry_function: Function, frame_mem: pointer): TaggedValue

########################################################################################################################
# Execution frames.                                                                                                    #
########################################################################################################################

type
  ## A frame represents the memory that the evaluation of a single function call requires. The memory for all frames is
  ## preallocated before `evaluate` is called.
  Frame = object
    function: Function
    registers: ptr UncheckedArray[TaggedValue]
  FramePtr = ptr Frame

## Don't move `create_frame` to a different module. GCC won't inline it.
# TODO (vm): We should possibly clear a frame by nilling all references so that pointers left in registers aren't
#            causing memory leaks. However, this also incurs a big performance penalty, so we should probably rather
#            verify first that this is a problem before fixing it.
proc create_frame(function: Function, frame_base: pointer): FramePtr {.inline.} =
  const preamble_size = sizeof(Frame)
  let frame = cast[FramePtr](frame_base)
  frame.function = function
  frame.registers = cast[ptr UncheckedArray[TaggedValue]](cast[uint](frame_base) + cast[uint](preamble_size))
  frame

## Initializes the `frame_*` size and offset stats of the given function.
proc init_frame_stats*(function: Function) =
  const preamble_size = sizeof(Frame)
  function.frame_size = cast[uint16](preamble_size + sizeof(TaggedValue) * cast[int](function.register_count))

########################################################################################################################
# Global variable functions.                                                                                           #
########################################################################################################################

## Gets the global variable's `value`. If the variable hasn't been initialized yet, `get_global` performs the
## initialization first and then returns the new value. `frame_mem` must be provided for the execution of the
## initialization function.
##
## Note that `get_global` cannot be placed in the module `definitions` because of a mutual dependency with `evaluate`.
proc get_global*(variable: GlobalVariable, frame_mem: pointer): TaggedValue =
  if not variable.is_initialized:
    variable.value = evaluate(variable.initializer, frame_mem)
    variable.is_initialized = true
  variable.value

## Sets the global variable's `value`, overwriting any previous value. Also sets `is_initialized` to true.
proc set_global*(variable: GlobalVariable, value: TaggedValue) =
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

template list_append(new_tpe): untyped =
  let list = reg_get_ref_arg(1, ListValue)
  let new_element = reg_get_arg(2)
  # This relies on `seq`'s deep copy semantics on assignment.
  var new_elements = list.elements
  new_elements.add(new_element)
  reg_set_ref_arg(0, new_list(new_elements, new_tpe))

template next_frame_base(): pointer = cast[pointer](cast[uint](frame) + frame.function.frame_size)

template call_start(target): FramePtr =
  let target_frame_base = next_frame_base()
  create_frame(target, target_frame_base)

template call_end(target_frame): untyped =
  # After function evaluation has finished, it must guarantee that the return value is in the first register.
  reg_set_arg(0, target_frame.registers[0])

## Calls a given Function target with one argument.
template call1(target, argument0): untyped =
  let target_frame = call_start(target)
  target_frame.registers[0] = argument0
  evaluate(target_frame)
  call_end(target_frame)

## Calls a given Function target with two arguments.
template call2(target, argument0, argument1): untyped =
  let target_frame = call_start(target)
  target_frame.registers[0] = argument0
  target_frame.registers[1] = argument1
  evaluate(target_frame)
  call_end(target_frame)

template dispatch1(mf): untyped =
  let argument0 = reg_get_arg(2)
  let target = get_dispatch_target(mf, argument0)
  call1(target, argument0)

template dispatch2(mf): untyped =
  let argument0 = reg_get_arg(2)
  let argument1 = reg_get_arg(3)
  let target = get_dispatch_target(mf, argument0, argument1)
  call2(target, argument0, argument1)

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
      var elements = new_seq_of_cap[TaggedValue](last - first + 1)
      for i in first .. last:
        elements.add(reg_get(i))
      reg_set_ref_arg(0, values.new_tuple(elements))

    of Operation.Tuple2:
      var elements: seq[TaggedValue]
      new_seq(elements, 2)
      elements[0] = reg_get_arg(1)
      elements[1] = reg_get_arg(2)
      reg_set_ref_arg(0, values.new_tuple(elements))

    of Operation.TupleGet:
      let tpl = reg_get_ref_arg(1, TupleValue)
      reg_set_arg(0, tpl.elements[instruction.arg(2)])

    of Operation.FunctionCall1:
      let function = reg_get_ref_arg(1, FunctionValue)
      if function.is_fixed:
        let argument0 = reg_get_arg(2)
        call1(cast[Function](function.target), argument0)
      else:
        dispatch1(cast[MultiFunction](function.target))

    of Operation.FunctionCall2:
      let function = reg_get_ref_arg(1, FunctionValue)
      if function.is_fixed:
        let argument0 = reg_get_arg(2)
        let argument1 = reg_get_arg(3)
        call2(cast[Function](function.target), argument0, argument1)
      else:
        dispatch2(cast[MultiFunction](function.target))

    of Operation.ListAppend:
      let new_tpe = const_types_arg(3)
      list_append(new_tpe)

    of Operation.ListAppendUntyped:
      let list = reg_get_ref_arg(1, ListValue)
      list_append(list.tpe)

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

    of Operation.GlobalGetEager:
      let gv = constants.global_variables[instruction.arg(1)]
      reg_set_arg(0, gv.value)

    of Operation.GlobalGetLazy:
      let gv = constants.global_variables[instruction.arg(1)]
      let frame_base = next_frame_base()
      let value = get_global(gv, frame_base)
      reg_set_arg(0, value)

    of Operation.GlobalSet:
      let gv = constants.global_variables[instruction.arg(0)]
      let value = reg_get_arg(1)
      set_global(gv, value)

    of Operation.Dispatch1:
      let mf = constants.multi_functions[instruction.arg(1)]
      dispatch1(mf)

    of Operation.Dispatch2:
      let mf = constants.multi_functions[instruction.arg(1)]
      dispatch2(mf)

    of Operation.Return:
      reg_set(0, reg_get_arg(0))
      break

    of Operation.Return0:
      break

proc evaluate*(entry_function: Function, frame_mem: pointer): TaggedValue =
  let frame = create_frame(entry_function, frame_mem)
  evaluate(frame)

  # The bytecode must ensure that the result is in the first register.
  reg_get(0)
