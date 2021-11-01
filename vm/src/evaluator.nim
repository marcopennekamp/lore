from bytecode import Operation, Instruction, Function
from values import Value, IntValue
from utils import when_debug

type
  ## A stack value is either a Value reference, an integer, or an unsigned integer (to represent booleans). The plain
  ## types exist for optimization within function calls and occasionally across function calls when a parameter is
  ## guaranteed to be e.g. an Int. Otherwise, non-references have to be boxed. Whether a StackValue is a reference or
  ## primitive is encoded in the bytecode.
  ##
  ## Note that when creating a StackValue, the generated C code will contain multiple assignments to each `*_value`
  ## field. GCC will optimize this and the resulting assembly will only contain a single assignment.
  StackValue {.union.} = object
    uint_value: uint64
    int_value: int64
    ref_value: Value

  ## A frame represents the memory that the evaluation of a single function call requires. The memory for all frames is
  ## preallocated before `evaluate` is called.
  Frame = object
    function: Function
    stack: ptr UncheckedArray[StackValue]
    locals: ptr UncheckedArray[StackValue]
  FramePtr = ptr Frame

# TODO (vm): Move this to `utils`.
template `+`(p: pointer, offset: uint): pointer = cast[pointer](cast[uint](p) + offset)

## Don't move `create_frame` to a different module. GCC won't inline it.
proc create_frame(function: Function, frame_mem: pointer, caller: FramePtr): FramePtr =
  let frame_base =
    if caller == nil: frame_mem
    else: cast[pointer](caller) + caller.function.frame_size

  let frame = cast[FramePtr](frame_base)
  frame.function = function
  frame.stack = cast[ptr UncheckedArray[StackValue]](frame_base + function.frame_stack_offset)
  frame.locals = cast[ptr UncheckedArray[StackValue]](frame_base + function.frame_locals_offset)

  frame

## Don't move `delete_frame` to a different module. GCC won't inline it.
proc delete_frame(frame: FramePtr) =
  let function = frame.function
  frame.function = nil

  # TODO (vm): We should possibly nil all references so that pointers left on the stack or locals array aren't causing
  #            memory leaks. However, this also incurs a big performance penalty, so we should probably rather verify
  #            first that this is a problem before fixing it.
  #zeroMem(frame.stack, (function.stack_size + function.locals_size) * cast[uint](sizeof(StackValue)))

## Initializes the `frame_*` size and offset stats of the given function.
proc init_frame_stats*(function: Function) =
  const preamble_size = sizeof(Frame)
  function.frame_size = cast[uint16](
    preamble_size +
    sizeof(uint64) * (cast[int](function.stack_size) + cast[int](function.locals_size))
  )
  function.frame_stack_offset = cast[uint16](preamble_size)
  function.frame_locals_offset = cast[uint16](function.frame_stack_offset + function.stack_size * cast[uint](sizeof(StackValue)))

template stack_push(stack_value): untyped =
  stack_index += 1
  frame.stack[stack_index] = stack_value

template stack_push_uint(value): untyped = stack_push(StackValue(uint_value: value))
template stack_push_int(value): untyped = stack_push(StackValue(int_value: value))
template stack_push_ref(value): untyped = stack_push(StackValue(ref_value: value))

template stack_pop(): untyped =
  let element = frame.stack[stack_index]
  stack_index -= 1
  element

template stack_pop_uint(): untyped = stack_pop().uint_value
template stack_pop_int(): untyped = stack_pop().int_value
template stack_pop_ref(tpe): untyped = cast[tpe](stack_pop().ref_value)

template stack_peek(): untyped = frame.stack[stack_index]

template stack_peek_uint(): untyped = stack_peek().uint_value
template stack_peek_int(): untyped = stack_peek().int_value
template stack_peek_ref(tpe): untyped = cast[tpe](stack_peek().ref_value)

template stack_assign_uint(value): untyped =
  frame.stack[stack_index].uint_value = value

template stack_assign_int(value): untyped =
  frame.stack[stack_index].int_value = value

template stack_assign_ref(value): untyped =
  frame.stack[stack_index].ref_value = value

# TODO (vm): Support entry arguments.
proc evaluate(frame: FramePtr, frame_mem: pointer) =
  when_debug: echo "Evaluating function ", frame.function.name, " at frame base ", cast[uint](frame)

  let code = frame.function.code
  let constants = frame.function.constants

  var pc: uint16 = 0
  var stack_index: int16 = -1

  while true:
    {.computedgoto.}
    let instruction = code[pc]
    when_debug: echo instruction.operation, " (PC: ", pc, ")"
    pc += 1

    case instruction.operation
    of Operation.LocalStore:
      let sv = stack_pop()
      frame.locals[instruction.arg0.uint_value] = sv

    of Operation.LocalLoad:
      let v = frame.locals[instruction.arg0.uint_value]
      stack_push(v)

    of Operation.IntPush:
      stack_push_int(instruction.arg0.int_value)

    of Operation.IntAdd:
      let b = stack_pop_int()
      stack_assign_int(stack_peek_int() + b)

    of Operation.IntSubtract:
      let b = stack_pop_int()
      stack_assign_int(stack_peek_int() - b)

    of Operation.IntLessThan:
      let b = stack_pop_int()
      let a = stack_peek_int()
      stack_assign_uint(if a < b: 1 else: 0)

    of Operation.IntBox:
      let v = stack_pop_int()
      stack_push_ref(values.new_int(v))

    of Operation.IntUnbox:
      let v = stack_pop_ref(IntValue)
      stack_push_int(v.value)

    of Operation.IntBoxPush:
      let v = values.new_int(instruction.arg0.int_value)
      stack_push_ref(v)

    of Operation.IntBoxAdd:
      let b = stack_pop_ref(IntValue)
      let a = stack_peek_ref(IntValue)
      stack_assign_ref(values.new_int(a.value + b.value))

    of Operation.Jump:
      pc = instruction.arg0.uint_value

    of Operation.JumpIfFalse:
      let predicate = stack_pop_uint()
      if (predicate == 0):
        pc = instruction.arg0.uint_value

    of Operation.JumpIfTrue:
      let predicate = stack_pop_uint()
      if (predicate != 0):
        pc = instruction.arg0.uint_value

    of Operation.Dispatch:
      let argument_count = instruction.arg0.uint_value
      let target = constants.functions[instruction.arg1.uint_value]
      let target_frame = create_frame(target, frame_mem, frame)
      when_debug: echo "Dispatch to target ", target.name, " with frame ", cast[uint](target_frame)

      # TODO (vm): We want to move the arguments from the stack into `arguments` as efficiently as possible. `copyMem`
      #            doesn't work with reference-counting garbage collection. However, if we switch to a traditional GC,
      #            this should be possible.
      var i: uint = 0
      while i < argument_count:
        target_frame.locals[i] = stack_pop()
        when_debug: echo "Argument: ", cast[IntValue](target_frame.locals[i].ref_value).value
        i += 1

      evaluate(target_frame, frame_mem)

      # After function evaluation has finished, it must guarantee that there is exactly one value on the stack.
      stack_push(target_frame.stack[0])

      delete_frame(target_frame)
      when_debug: echo "Finished dispatch to target ", target.name, "with frame ", cast[uint](target_frame)

    of Operation.Return:
      break

proc evaluate*(entry_function: Function, frame_mem: pointer): Value =
  let frame = create_frame(entry_function, frame_mem, nil)
  evaluate(frame, frame_mem)

  # The bytecode must ensure that there is exactly one Value on the reference stack.
  result = cast[Value](frame.stack[0])

  delete_frame(frame)
