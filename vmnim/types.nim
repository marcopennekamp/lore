type
  Kind* = enum
    TypeVariable
    Any
    Nothing
    Int
    Real
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

  # TODO (vm): Turn these into a real object hierarchy. Object variants are apparently represented as C unions, which
  #            is not memory-efficient here.
  TypeObj = object
    case kind: Kind
    of TypeVariable:
      lower_bound, upper_bound: Type
    of Any: discard
    of Nothing: discard
    of Int: discard
    of Real: discard
    of Boolean: discard
    of String: discard
    of Sum: discard
    of Intersection: discard
    of Tuple: discard
    of Function: discard
    of List: discard
    of Map: discard
    of Shape: discard
    of Symbol: discard
    of Trait: discard
    of Struct: discard

  Type* = ref TypeObj

let
  any* = Type(kind: Any)
  nothing* = Type(kind: Nothing)
  int* = Type(kind: Int)
  real* = Type(kind: Real)
  boolean* = Type(kind: Boolean)
  string* = Type(kind: String)
