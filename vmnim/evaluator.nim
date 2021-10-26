from bytecode import Operation, Instruction
from values import Value, IntValue

template stack_push(value): untyped =
  stack_index += 1
  stack[stack_index] = value

template stack_pop(tpe): untyped =
  let element = cast[tpe](stack[stack_index])
  stack_index -= 1
  element

proc evaluate*(code: seq[Instruction]): Value =
  var index: uint = 0
  var stack: array[16, Value]
  var stack_index = -1

  while true:
    let instruction = code[index]
    index += 1

    case instruction.operation
    of Operation.IntAdd:
      let v2 = stack_pop(IntValue)
      let v1 = stack_pop(IntValue)
      let n = v1.value + v2.value
      stack_push(values.new_int(n))

    of Operation.IntPush:
      stack_push(values.new_int(instruction.arg0.int_value))

    of Operation.Return:
      break

  if stack.len > 0:
    stack_pop(Value)
  else:
    nil
