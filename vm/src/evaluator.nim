from bytecode import Operation, Instruction, Function
from values import Value, IntValue
from utils import when_debug

type
  # A stack value is either a plain int, float, or a reference to a more complex Value. The plain types exist for
  # optimization within function calls. When other multi-functions are called, Ints have to be boxed, and
  # multi-functions also always return a boxed Value. Whether a StackValue is a reference, int, or real is entirely
  # encoded in the executed bytecode.
  #
  # Further optimizing boxing and unboxing across function calls is possible, but requires certain conditions to be
  # met, such as an argument always being an int/real regardless of the function that dispatch might choose. This will
  # require optimizations both in the Lore compiler and in the VM.
  #
  # Note that when creating a StackValue, the generated C code will contain multiple assignments to each `*_value`
  # field. GCC will optimize this and the resulting assembly will only contain a single assignment.
  #[
  StackValue {.union.} = object
    ref_value: Value
    int_value: int64
    real_value: float64
  ]#

  Primitive {.union.} = object
    uint_value: uint64
    int_value: int64

  ## A frame represents the memory that the evaluation of a single function call requires. The memory for all frames is
  ## preallocated when `evaluate` is first called. Frames are then managed using a frame stack, with frames being
  ## pushed on and popped off when functions are called or finished.
  ##
  ## Due to Nim's reference counting mechanics, references are placed on a separate stack, so that (1) reference
  ## mechanics are properly inserted and (2) values other than 0 aren't accidentally interpreted as references, which
  ## would usually lead to a segfault when Nim attempts to increment or decrement the reference counter.
  ##
  ## When a frame is deleted, its reference memory must be nilled, so that reference counts can be properly updated.
  Frame = object
    function: Function
    stack: ptr UncheckedArray[Primitive]
    locals: ptr UncheckedArray[Primitive]
    # TODO (vm): This probably only works with reference counting, because with a classic GC, the GC will never see the
    #            pointers in these arrays, as the frame memory is allocated using `alloc`. On the other hand, the Boehm
    #            GC for example scans the stack and will absolutely find the frame memory. However, it probably won't
    #            FOLLOW it because it'll be outside the heap bounds. If we're using the Boehm collector, perhaps a
    #            solution would be to allocate the frame memory via the GC's malloc function.
    #            The reference counting requires us to nil each reference individually, so that appropriate reference
    #            counter update code can be inserted by the Nim compiler. If we use a GC, we should still zero the
    #            frame's references and the pointer to `function` (so that unused objects are properly collected).
    #            However, we will be able to use built-in memory functions, which will be faster.
    # TODO (vm): When using Boehm GC, we might be able to get away with a single stack and a single locals array, and
    #            then use stack values. The only reason we're separating primitives and references is because reference
    #            counting continually segfaults. Also, because we have to "nil and dec" all references, this requires
    #            us to either know the type of each value on the stack OR separate primitives and references. Hence, if
    #            we merge these two concepts, it'll be impossible to offer reference counting for Lore. This is doubly
    #            true because bytecode semantics will change based on having one or two stacks. Pushed elements might
    #            be in the wrong order if we suddenly merge the stacks.
    ref_stack: ptr UncheckedArray[Value]
    ref_locals: ptr UncheckedArray[Value]
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
  frame.stack = cast[ptr UncheckedArray[Primitive]](frame_base + function.frame_stack_offset)
  frame.locals = cast[ptr UncheckedArray[Primitive]](frame_base + function.frame_locals_offset)
  frame.ref_stack = cast[ptr UncheckedArray[Value]](frame_base + function.frame_ref_stack_offset)
  frame.ref_locals = cast[ptr UncheckedArray[Value]](frame_base + function.frame_ref_locals_offset)

  frame

## Don't move `delete_frame` to a different module. GCC won't inline it.
proc delete_frame(frame: FramePtr) =
  let function = frame.function
  frame.function = nil

  # TODO (vm): We should possibly nil all references so that pointers left on the stack or locals array aren't causing
  #            memory leaks. However, this also incurs a big performance penalty, so we should probably rather verify
  #            first that this is a problem before fixing it.
  #zeroMem(frame.ref_stack, (function.ref_stack_size + function.ref_locals_size) * cast[uint](sizeof(Value)))

## Initializes the `frame_*` size and offset stats of the given function.
proc init_frame_stats*(function: Function) =
  const preamble_size = sizeof(Frame)
  function.frame_size = cast[uint16](
    preamble_size +
    sizeof(uint64) * (cast[int](function.stack_size) + cast[int](function.locals_size)) +
    sizeof(Value) * (cast[int](function.ref_stack_size) + cast[int](function.ref_locals_size))
  )
  function.frame_stack_offset = cast[uint16](preamble_size)
  function.frame_locals_offset = cast[uint16](function.frame_stack_offset + function.stack_size * cast[uint](sizeof(Primitive)))
  function.frame_ref_stack_offset = cast[uint16](function.frame_locals_offset + function.locals_size * cast[uint](sizeof(Primitive)))
  function.frame_ref_locals_offset = cast[uint16](function.frame_ref_stack_offset + function.ref_stack_size * cast[uint](sizeof(Primitive)))

template stack_push(primitive): untyped =
  stack_index += 1
  frame.stack[stack_index] = primitive

template stack_push_uint(value): untyped = stack_push(Primitive(uint_value: value))
template stack_push_int(value): untyped = stack_push(Primitive(int_value: value))

template stack_pop(): untyped =
  let element = frame.stack[stack_index]
  stack_index -= 1
  element

template stack_pop_uint(): untyped = stack_pop().uint_value
template stack_pop_int(): untyped = stack_pop().int_value

template stack_peek(): untyped = frame.stack[stack_index]

template stack_peek_uint(): untyped = stack_peek().uint_value
template stack_peek_int(): untyped = stack_peek().int_value

template stack_assign_uint(value): untyped =
  frame.stack[stack_index].uint_value = value

template stack_assign_int(value): untyped =
  frame.stack[stack_index].int_value = value

template ref_stack_push(value): untyped =
    ref_stack_index += 1
    frame.ref_stack[ref_stack_index] = value

template ref_stack_pop(tpe): untyped =
  let element = frame.ref_stack[ref_stack_index]
  ref_stack_index -= 1
  cast[tpe](element)

template ref_stack_peek(tpe): untyped = cast[tpe](frame.ref_stack[ref_stack_index])

template ref_stack_assign(value): untyped =
  frame.ref_stack[ref_stack_index] = value

#[
template wrap_ref(value): untyped = StackValue(ref_value: value)
template stack_push_ref(value): untyped = stack_push(wrap_ref(value))
template stack_pop_ref(tpe): untyped = cast[tpe](stack_pop().ref_value)
template stack_peek_ref(tpe): untyped = cast[tpe](stack_peek().ref_value)

template wrap_int(value): untyped = StackValue(int_value: value)
template stack_push_int(value): untyped = stack_push(wrap_int(value))
template stack_pop_int(): untyped = stack_pop().int_value
template stack_peek_int(): untyped = stack_peek().int_value
]#

# TODO (vm): `{.push checks: off.}` (or for the whole file) if compiling with `-d:release` only instead of danger.
# TODO (vm): Support entry arguments.
## `evaluate` does not yet support garbage collection. This includes the default `refc` memory management, which has a
## backup garbage collector. The GC won't see the value references contained in `frame_mem` and incorrectly free the
## memory associated with these values.
proc evaluate(frame: FramePtr, frame_mem: pointer) =
  when_debug: echo "Evaluating function ", frame.function.name

  let code = frame.function.code
  let constants = frame.function.constants

  var pc: uint16 = 0
  var stack_index: int16 = -1
  var ref_stack_index: int16 = -1

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

    of Operation.RefLocalStore:
      let sv = ref_stack_pop(Value)
      frame.ref_locals[instruction.arg0.uint_value] = sv

    of Operation.RefLocalLoad:
      let v = frame.ref_locals[instruction.arg0.uint_value]
      ref_stack_push(v)

    of Operation.IntPush:
      stack_push_int(instruction.arg0.int_value)

    of Operation.IntAdd:
      let b = stack_pop_int()
      stack_assign_int(stack_peek_int() + b)

    of Operation.IntSubtract:
      let b = stack_pop_int()
      stack_assign_int(stack_peek_int() - b)

    of Operation.IntLessThan:
      let b = stack_pop_uint()
      let a = stack_peek_uint()
      stack_assign_uint(if a < b: 1 else: 0)

    of Operation.IntBox:
      let v = stack_pop_int()
      ref_stack_push(values.new_int(v))

    of Operation.IntUnbox:
      let v = ref_stack_pop(IntValue)
      stack_push_int(v.value)

    of Operation.IntBoxPush:
      let v = values.new_int(instruction.arg0.int_value)
      ref_stack_push(v)

    of Operation.IntBoxAdd:
      let b = ref_stack_pop(IntValue)
      let a = ref_stack_peek(IntValue)
      ref_stack_assign(values.new_int(a.value + b.value))

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
      when_debug: echo "Dispatch to target ", target.name

      let target_frame = create_frame(target, frame_mem, frame)

      # TODO (vm): We want to move the arguments from the stack into `arguments` as efficiently as possible. `copyMem`
      #            doesn't work with reference-counting garbage collection. However, if we switch to a traditional GC,
      #            this should be
      #            possible.
      var i: uint = 0
      while i < argument_count:
        target_frame.ref_locals[i] = ref_stack_pop(Value)
        i += 1

      evaluate(target_frame, frame_mem)

      # After function evaluation has finished, it must guarantee that there is exactly one reference on the stack.
      ref_stack_push(target_frame.ref_stack[0])

      delete_frame(target_frame)

    of Operation.Return:
      break

proc evaluate*(entry_function: Function, frame_mem: pointer): Value =
  let frame = create_frame(entry_function, frame_mem, nil)
  evaluate(frame, frame_mem)

  # The bytecode must ensure that there is exactly one Value on the reference stack.
  result = frame.ref_stack[0]

  delete_frame(frame)
