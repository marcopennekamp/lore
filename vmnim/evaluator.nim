from bytecode import Operation, Instruction
from values import Value, IntValue

type
  # TODO (vm): This only works well on 64-bit machines.
  # A stack value is either a plain int, float, or a reference to a more complex Value. The plain types exist for
  # optimization within function calls. When other multi-functions are called, Ints have to be boxed, and
  # multi-functions also always return a boxed Value. Whether a StackValue is a reference, int, or real is entirely
  # encoded in the executed bytecode.
  # Further optimizing boxing and unboxing across function calls is possible, but requires certain conditions to be
  # met, such as an argument always being an int/real regardless of the function that dispatch might choose. This will
  # require optimizations both in the Lore compiler and in the VM.
  StackValue {.union.} = object
    ref_value: Value
    int_value: int64
    real_value: float64

template stack_push_ref(value): untyped =
  stack_index += 1
  stack[stack_index] = StackValue(ref_value: value)

template stack_pop_ref(tpe): untyped =
  let element = cast[tpe](stack[stack_index].ref_value)
  stack_index -= 1
  element

template stack_push_int(value): untyped =
  stack_index += 1
  stack[stack_index] = StackValue(int_value: value)

template stack_pop_int(): untyped =
  let element = stack[stack_index].int_value
  stack_index -= 1
  element

proc evaluate*(code: seq[Instruction]): Value =
  var index: uint = 0
  var stack: array[16, StackValue]
  var stack_index = -1

  while true:
    let instruction = code[index]
    index += 1

    case instruction.operation
    of Operation.IntAdd:
      let b = stack_pop_int()
      stack[stack_index].int_value += b

    of Operation.IntPush:
      stack_push_int(instruction.arg0.int_value)

    of Operation.IntBox:
      let v = stack_pop_int()
      stack_push_ref(values.new_int(v))

    of Operation.IntBoxPush:
      stack_push_ref(values.new_int(instruction.arg0.int_value))

    of Operation.IntUnbox:
      let v = stack_pop_ref(IntValue)
      stack_push_int(v.value)

    of Operation.Return:
      break

  # The bytecode must ensure that the return value on the stack is a boxed Value.
  if stack.len > 0:
    stack_pop_ref(Value)
  else:
    nil
