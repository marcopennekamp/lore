from functions import Function, get_dispatch_target
from instructions import Operation, Instruction
from values import TaggedValue, Value, tag_reference, untag_reference, tag_int, untag_int, tag_boolean, untag_boolean
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

#template reg_get_ref(index, tpe): untyped = cast[tpe](untag_reference(reg_get(index)))
template reg_get_int(index): untyped = untag_int(reg_get(index))
template reg_get_bool(index): untyped = untag_boolean(reg_get(index))

#template reg_get_ref_arg1(tpe): untyped = reg_get_ref(instruction.arg1.uint_value, tpe)
template reg_get_int_arg1(): untyped = reg_get_int(instruction.arg1.uint_value)
template reg_get_bool_arg1(): untyped = reg_get_bool(instruction.arg1.uint_value)

#template reg_get_ref_arg2(tpe): untyped = reg_get_ref(instruction.arg2.uint_value, tpe)
template reg_get_int_arg2(): untyped = reg_get_int(instruction.arg2.uint_value)

template reg_set(target_index, register_value): untyped =
  frame.registers[target_index] = register_value

#template reg_set_ref(target_index, value): untyped = reg_set(target_index, tag_reference(value))
template reg_set_int(target_index, value): untyped = reg_set(target_index, tag_int(value))
template reg_set_bool(target_index, value): untyped = reg_set(target_index, tag_boolean(value))

#template reg_set_ref_arg0(value): untyped = reg_set_ref(instruction.arg0.uint_value, value)
template reg_set_int_arg0(value): untyped = reg_set_int(instruction.arg0.uint_value, value)
template reg_set_bool_arg0(value): untyped = reg_set_bool(instruction.arg0.uint_value, value)

proc evaluate(frame: FramePtr) =
  when_debug: echo "Evaluating function ", frame.function.multi_function.name, " with frame ", cast[uint](frame)

  let code = frame.function.code
  let constants = frame.function.constants

  var pc: uint16 = 0

  while true:
    {.computedgoto.}
    let instruction = code[pc]
    when_debug: echo instruction.operation, " (PC: ", pc, ")"
    pc += 1

    case instruction.operation
    of Operation.IntConst:
      reg_set_int_arg0(instruction.arg1.int_value)

    of Operation.IntAdd:
      let a = reg_get_int_arg1()
      let b = reg_get_int_arg2()
      reg_set_int_arg0(a + b)

    of Operation.IntAddConst:
      let a = reg_get_int_arg1()
      let b = instruction.arg2.int_value
      reg_set_int_arg0(a + b)

    of Operation.IntSubConst:
      let a = reg_get_int_arg1()
      let b = instruction.arg2.int_value
      reg_set_int_arg0(a - b)

    of Operation.IntGtConst:
      let a = reg_get_int_arg1()
      let b = instruction.arg2.int_value
      reg_set_bool_arg0(a > b)

    of Operation.Jump:
      pc = instruction.arg0.uint_value

    of Operation.JumpIfFalse:
      let predicate = reg_get_bool_arg1()
      if (not predicate):
        pc = instruction.arg0.uint_value

    of Operation.JumpIfTrue:
      let predicate = reg_get_bool_arg1()
      if (predicate):
        pc = instruction.arg0.uint_value

    of Operation.Dispatch1:
      let mf = constants.multi_functions[instruction.arg1.uint_value]
      let argument0 = reg_get(instruction.arg2.uint_value)

      let target = get_dispatch_target(mf, argument0)
      let target_frame_base = cast[pointer](cast[uint](frame) + frame.function.frame_size)
      let target_frame = create_frame(target, target_frame_base)
      when_debug: echo "Dispatch to target ", mf.name, " with frame ", cast[uint](target_frame)

      target_frame.registers[0] = argument0
      evaluate(target_frame)

      # After function evaluation has finished, it must guarantee that the return value is in the first register.
      reg_set(instruction.arg0.uint_value, target_frame.registers[0])

      when_debug: echo "Finished dispatch to target ", mf.name, " with frame ", cast[uint](target_frame)

    of Operation.Return:
      reg_set(0, reg_get(instruction.arg0.uint_value))
      break

    of Operation.Return0:
      break

proc evaluate*(entry_function: Function, frame_mem: pointer): TaggedValue =
  let frame = create_frame(entry_function, frame_mem)
  evaluate(frame)

  # The bytecode must ensure that the result is in the first register.
  reg_get(0)
