module Lore::Bytecode
  enum Operation : UInt8
    IntAdd
    IntPush
    Return
  end

  class Instruction
    getter operation, arg0, arg1
    
    def initialize(@operation : Operation, @arg0 : UInt16, @arg1 : UInt16)
    end

    def arg0_int : Int64
      (0_i16 | arg0).to_i64
    end
  end
end
