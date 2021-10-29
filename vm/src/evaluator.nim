from bytecode import Operation, Instruction, Function
from values import Value, IntValue
from utils import when_debug

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

template stack_push(stack_value): untyped =
  stack_index += 1
  stack[stack_index] = stack_value

template stack_pop(): untyped =
  let element = stack[stack_index]
  stack_index -= 1
  element

template stack_peek(): untyped = stack[stack_index]

template wrap_ref(value): untyped = StackValue(ref_value: value)
template stack_push_ref(value): untyped = stack_push(wrap_ref(value))
template stack_pop_ref(tpe): untyped = cast[tpe](stack_pop().ref_value)
template stack_peek_ref(tpe): untyped = cast[tpe](stack_peek().ref_value)

template wrap_int(value): untyped = StackValue(int_value: value)
template stack_push_int(value): untyped = stack_push(wrap_int(value))
template stack_pop_int(): untyped = stack_pop().int_value
template stack_peek_int(): untyped = stack_peek().int_value

var
  # The `arguments` sequence is a global store of function arguments that expands and shrinks as functions are
  # evaluated. This allows passing function arguments without allocating intermediate sequences. The actual shrinking
  # and growing is handled manually so that the sequence isn't reallocated too often.
  arguments: seq[Value] = newSeq[Value](256)

proc evaluate(function: Function, arguments_bc: uint): Value

proc evaluate*(function: Function, args: seq[Value]): Value =
  var i = 0
  for argument in args:
    arguments[i] = argument
    i += 1
  evaluate(function, 0)

# TODO (vm): `{.push checks: off.}` (or for the whole file) if compiling with `-d:release` only instead of danger.
# TODO (vm): Use `{.computedgoto.}` for the case statement.
# `arguments_bc`: The starting index of this function's `arguments` segment. ("Arguments base counter")
proc evaluate(function: Function, arguments_bc: uint): Value =
  let code = function.code
  var pc: uint = 0
  var stack {.noinit.}: array[16, StackValue]
  var stack_index = -1
  var locals {.noinit.}: array[16, StackValue]

  let arguments_next_bc = arguments_bc + function.arguments_count

  when_debug: echo "Evaluating function ", function.name

  while true:
    {.computedgoto.}
    let instruction = code[pc]
    when_debug: echo instruction.operation, " (PC: ", pc, ")"
    pc += 1

    case instruction.operation
    of Operation.ArgumentLoad:
      let v = arguments[arguments_bc + instruction.arg0.uint_value]
      stack_push_ref(v)

    of Operation.LocalStore:
      let sv = stack_pop()
      locals[instruction.arg0.uint_value] = sv

    of Operation.LocalLoad:
      let v = locals[instruction.arg0.uint_value]
      stack_push(v)

    of Operation.IntPush:
      stack_push_int(instruction.arg0.int_value)

    of Operation.IntAdd:
      let b = stack_pop_int()
      stack[stack_index].int_value += b

    of Operation.IntSubtract:
      let b = stack_pop_int()
      stack[stack_index].int_value -= b

    of Operation.IntLessThan:
      let b = stack_pop_int()
      let a = stack_peek_int()
      stack[stack_index].int_value = if a < b: 1 else: 0

    of Operation.IntBox:
      let v = stack_pop_int()
      stack_push_ref(values.new_int(v))

    of Operation.IntUnbox:
      let v = stack_pop_ref(IntValue)
      stack_push_int(v.value)

    of Operation.IntBoxPush:
      stack_push_ref(values.new_int(instruction.arg0.int_value))

    of Operation.IntBoxAdd:
      let b = stack_pop_ref(IntValue)
      let a = stack_peek_ref(IntValue)
      stack[stack_index] = wrap_ref(values.new_int(a.value + b.value))

    of Operation.Jump:
      pc = instruction.arg0.uint_value

    of Operation.JumpIfFalse:
      let predicate = stack_pop_int()
      if (predicate == 0):
        pc = instruction.arg0.uint_value

    of Operation.JumpIfTrue:
      let predicate = stack_pop_int()
      if (predicate != 0):
        pc = instruction.arg0.uint_value

    of Operation.Dispatch:
      let argument_count = instruction.arg0.uint_value
      let target = function.constants.functions[instruction.arg1.uint_value]
      when_debug: echo "Dispatch to target ", target.name

      # We should only increase the length of `arguments` if absolutely needed, and if so by a large margin.
      if arguments_next_bc + argument_count > cast[uint](arguments.len):
        arguments.set_len(arguments.len + 128)

      # TODO (vm): We want to move the arguments from the stack into `arguments` as efficiently as possible. `copyMem`
      #            doesn't work with a reference-counting GC. Can we optimize this?
      var i: uint = 0
      while i < argument_count:
        arguments[arguments_next_bc + i] = stack_pop_ref(Value)
        i += 1

      let output = evaluate(target, arguments_next_bc)
      stack_push_ref(output)

    of Operation.Return:
      break

  # We have to conservatively shrink the arguments sequence again if its usage drops below 50%.
  if cast[uint](arguments.len) < arguments_next_bc * 2:
    arguments.set_len(arguments_next_bc)

  # The bytecode must ensure that the return value on the stack is a boxed Value.
  if stack.len > 0:
    stack_pop_ref(Value)
  else:
    nil
