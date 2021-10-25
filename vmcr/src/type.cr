module Lore 
  enum Kind
    TypeVariable
    Any
    Nothing
    Real
    Int
    Boolean
    String
    Sum
    Intersection
    Tuple
    Function
    List
    Map
    Shape
    Symbol
    Trait
    Struct
  end

  abstract class Type
  end

  module Types 
    class TypeVariable < Type
      getter lower_bound, upper_bound

      def initialize(@lower_bound : Type, @upper_bound : Type)
      end
    end

    class AnyType < Type end
    class NothingType < Type end
    class RealType < Type end
    class IntType < Type end
    class BooleanType < Type end
    class StringType < Type end

    ANY = AnyType.new
    NOTHING = NothingType.new
    REAL = RealType.new
    INT = IntType.new
    BOOLEAN = BooleanType.new
    STRING = StringType.new
  end
end
