from dispatch import get_dispatch_target
from functions import Function
import instructions
import values
from utils import when_debug

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

template const_value(index): untyped = constants.values[index]
template const_value_arg(index): untyped = const_value(instruction.arg(index))

template const_value_ref(index, tpe): untyped = untag_reference(const_value(index), tpe)
template const_value_ref_arg(index, tpe): untyped = const_value_ref(instruction.arg(index), tpe)

template dispatch_start(mf, target): FramePtr =
  let target_frame_base = cast[pointer](cast[uint](frame) + frame.function.frame_size)
  let target_frame = create_frame(target, target_frame_base)
  when_debug: echo "Dispatch to target ", mf.name, " with frame ", cast[uint](target_frame)
  target_frame

template dispatch_end(mf, target_frame): untyped =
  # After function evaluation has finished, it must guarantee that the return value is in the first register.
  reg_set_arg(0, target_frame.registers[0])
  when_debug: echo "Finished dispatch to target ", mf.name, " with frame ", cast[uint](target_frame)

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

    of Operation.Dispatch1:
      let mf = constants.multi_functions[instruction.arg(1)]
      let argument0 = reg_get_arg(2)
      let target = get_dispatch_target(mf, argument0)
      let target_frame = dispatch_start(mf, target)
      target_frame.registers[0] = argument0
      evaluate(target_frame)
      dispatch_end(mf, target_frame)

    of Operation.Dispatch2:
      let mf = constants.multi_functions[instruction.arg(1)]
      let argument0 = reg_get_arg(2)
      let argument1 = reg_get_arg(3)
      let target = get_dispatch_target(mf, argument0, argument1)
      let target_frame = dispatch_start(mf, target)
      target_frame.registers[0] = argument0
      target_frame.registers[1] = argument1
      evaluate(target_frame)
      dispatch_end(mf, target_frame)

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
