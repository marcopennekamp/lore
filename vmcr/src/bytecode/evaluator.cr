require "../type.cr"
require "../value.cr"

include Lore::Values

module Lore::Bytecode::Evaluator
  extend self

  def evaluate(code : Array(Instruction))
    index = 0
    stack = uninitialized Value[16]
    stack_index = -1

    loop do
      instruction = code.unsafe_fetch(index)
      index += 1

      case instruction.operation
      when Operation::IntAdd
        v2 = stack_pop(IntValue)
        v1 = stack_pop(IntValue)
        result = v1.value + v2.value
        stack_push(IntValue.new(result))

      when Operation::IntPush
        number = instruction.arg0_int
        stack_push(IntValue.new(number))

      when Operation::Return
        break
      end
    end

    if stack.size > 0
      stack_pop(Value)
    else
      nil
    end
  end

  macro stack_push(value)
    stack_index += 1
    stack[stack_index] = {{value}}
  end

  macro stack_pop(type)
    # The pointer cast circumvents the runtime type check that would normally be performed with `.as`.
    element = Pointer(Void).new(stack[stack_index].object_id).as({{type}})
    stack_index -= 1
    element
  end
end
