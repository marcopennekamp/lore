require "benchmark"

require "./bytecode/evaluator.cr"
require "./bytecode/instruction.cr"

include Lore::Bytecode

module Lore
  class VM
    def run
      code = [
        Instruction.new(Operation::IntPush, 1, 0),
        Instruction.new(Operation::IntPush, 2, 0),
        Instruction.new(Operation::IntAdd, 0, 0),
        Instruction.new(Operation::IntPush, 3, 0),
        Instruction.new(Operation::IntPush, 4, 0),
        Instruction.new(Operation::IntAdd, 0, 0),
        Instruction.new(Operation::IntAdd, 0, 0),
        Instruction.new(Operation::IntPush, 0xffff, 0), # 0xffff is -1 as an Int16.
        Instruction.new(Operation::IntAdd, 0, 0),
        Instruction.new(Operation::Return, 0, 0),
      ]

      Benchmark.ips do |x|
        x.report("VM run") { Evaluator.evaluate(code) }
      end

      Evaluator.evaluate(code)
    end
  end
end

vm = Lore::VM.new
result = vm.run
puts result
