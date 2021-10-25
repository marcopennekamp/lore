require "./type.cr"

module Lore
  abstract class Value
    abstract def type : Type
  end
  
  module Values
    class IntValue < Value
      getter value
  
      def initialize(@value : Int64)
      end

      def type : Types::IntType
        Types::INT
      end

      def to_s(io)
        io << value
      end
    end
  end
end
