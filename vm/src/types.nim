type
  Kind* {.pure.} = enum
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

  # Types are considered immutable and should never be mutated once they're attached to a value. Types must also be
  # acyclic, resembling a tree structure.
  # The pragmas `inheritable` and `pure` omit the `m_type` pointer from each type instance. This type tag is usually
  # used to discriminate between types at run time. However, `kind` already sufficiently distinguishes type instances.
  # A type which contains a sequence or string should be marked `shallow`, to optimize copying. Types which themselves
  # contain other types should be marked `acyclic`, to optimize garbage collection.
  Type* {.inheritable, pure.} = ref object
    kind*: Kind

  TypeVariable* {.pure, acyclic.} = ref object of Type
    lower_bound*, upper_bound*: Type

  # TODO (vm): Perhaps different types for different arities, such as Tuple2/3/4, Sum2/3/4, etc. Of course, this is
  #            trivially beneficial for tuples, because a 2-tuple will never be a sub- nor supertype of a 3-tuple.
  #            However, a 2-sum can easily be a subtype of a 3-sum.
  SumType* {.pure, shallow, acyclic.} = ref object of Type
    parts*: seq[Type]

  IntersectionType* {.pure, shallow, acyclic.} = ref object of Type
    parts*: seq[Type]

  TupleType* {.pure, shallow, acyclic.} = ref object of Type
    elements*: seq[Type]

  FunctionType* {.pure, acyclic.} = ref object of Type
    input*, output*: Type

  ListType* {.pure, acyclic.} = ref object of Type
    element*: Type

  MapType {.pure, acyclic.} = ref object of Type
    key*, value*: Type

  # TODO (vm): Implement shape types.

  SymbolType* {.pure, shallow.} = ref object of Type
    name*: string

  # TODO (vm): Implement trait and struct types.

let
  any* = Type(kind: Any)
  nothing* = Type(kind: Nothing)
  int* = Type(kind: Int)
  real* = Type(kind: Real)
  boolean* = Type(kind: Boolean)
  string* = Type(kind: String)

proc sum*(parts: open_array[Type]): SumType = SumType(kind: Kind.Sum, parts: @parts)
proc intersection*(parts: open_array[Type]): IntersectionType = IntersectionType(kind: Kind.Intersection, parts: @parts)
proc tpl*(elements: open_array[Type]): TupleType = TupleType(kind: Kind.Tuple, elements: @elements)
proc list*(element: Type): ListType = ListType(kind: Kind.List, element: element)
proc map*(key: Type, value: Type): MapType = MapType(kind: Kind.Map, key: key, value: value)


proc has_equal_in(ts1: seq[Type], ts2: seq[Type]): bool
proc are_exactly_equal(ts1: seq[Type], ts2: seq[Type]): bool

# TODO (vm): `{.push checks: off.}` if compiling with `-d:release` only instead of danger.
proc are_equal*(t1: Type, t2: Type): bool =
  # If the two types are referentially equal, they are obviously the same!
  if t1 == t2:
    return true

  if t1.kind != t2.kind:
    return false

  case t1.kind
  of Kind.TypeVariable: false # Type variables can only be referentially equal.
  of Kind.Any: true
  of Kind.Nothing: true
  of Kind.Real: true
  of Kind.Int: true
  of Kind.Boolean: true
  of Kind.String: true

  of Kind.Sum:
    let s1 = cast[SumType](t1)
    let s2 = cast[SumType](t2)
    has_equal_in(s1.parts, s2.parts) and has_equal_in(s2.parts, s1.parts)

  of Kind.Intersection:
    let i1 = cast[IntersectionType](t1)
    let i2 = cast[IntersectionType](t2)
    has_equal_in(i1.parts, i2.parts) and has_equal_in(i2.parts, i1.parts)

  of Kind.Tuple:
    let tp1 = cast[TupleType](t1)
    let tp2 = cast[TupleType](t2)
    are_exactly_equal(tp1.elements, tp2.elements)

  of Kind.Function:
    let f1 = cast[FunctionType](t1)
    let f2 = cast[FunctionType](t2)
    are_equal(f1.input, f2.input) and are_equal(f1.output, f2.output)

  of Kind.List:
    let l1 = cast[ListType](t1)
    let l2 = cast[ListType](t2)
    are_equal(l1.element, l2.element)

  of Kind.Map:
    let m1 = cast[MapType](t1)
    let m2 = cast[MapType](t2)
    are_equal(m1.key, m2.key) and are_equal(m1.value, m2.value)

  of Kind.Symbol:
    let s1 = cast[SymbolType](t1)
    let s2 = cast[SymbolType](t2)
    s1.name == s2.name

  else: false

proc has_equal_in(ts1: seq[Type], ts2: seq[Type]): bool =
  for t1 in ts1:
    var found = false
    for t2 in ts2:
      if are_equal(t1, t2):
        found = true
    if not found: return false
  true

proc are_exactly_equal(ts1: seq[Type], ts2: seq[Type]): bool =
  if ts1.len != ts2.len: return false
  var i = 0
  while i < ts1.len:
    if not are_equal(ts1[i], ts2[i]): return false
    i += 1
  true


when is_main_module:
  from utils import benchmark

  let sum1 = sum([string, int, boolean])
  let sum2 = sum([string, int, boolean])
  let sum3 = sum([real, boolean])

  echo are_equal(sum1, sum1)
  benchmark("sum1 == sum1", 100_000_000):
    discard are_equal(sum1, sum1)

  echo are_equal(sum1, sum2)
  benchmark("sum1 == sum2", 100_000_000):
    discard are_equal(sum1, sum2)

  echo are_equal(sum1, sum2)
  benchmark("sum1 == sum3", 100_000_000):
    discard are_equal(sum1, sum3)

  let tuple1 = tpl([
    sum([string, int, boolean]),
    intersection([string, int, boolean]),
    list(map(string, int)),
  ])

  let tuple2 = tpl([
    sum([string, int, boolean]),
    intersection([string, int, boolean]),
    list(map(string, int)),
  ])

  echo are_equal(tuple1, tuple2)
  benchmark("tuple1 == tuple2", 100_000_000):
    discard are_equal(tuple1, tuple2)

  benchmark("tuple3 == tuple4 (+creation)", 10_000_000):
    let tuple3 = tpl([
      sum([string, int, boolean]),
      intersection([string, int, boolean]),
      list(map(string, int)),
    ])

    let tuple4 = tpl([
      sum([string, int, boolean]),
      intersection([string, int, boolean]),
      list(map(string, int)),
    ])

    discard are_equal(tuple3, tuple4)
