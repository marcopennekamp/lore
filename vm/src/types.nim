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
  unit* = TupleType(kind: Kind.Tuple, elements: @[])

proc sum*(parts: open_array[Type]): SumType = SumType(kind: Kind.Sum, parts: @parts)
proc intersection*(parts: open_array[Type]): IntersectionType = IntersectionType(kind: Kind.Intersection, parts: @parts)
proc tpl*(elements: open_array[Type]): TupleType = TupleType(kind: Kind.Tuple, elements: @elements)
proc list*(element: Type): ListType = ListType(kind: Kind.List, element: element)
proc map*(key: Type, value: Type): MapType = MapType(kind: Kind.Map, key: key, value: value)

# TODO (vm): Intern symbol types.
proc symbol*(name: string): SymbolType = SymbolType(kind: Kind.Symbol, name: name)

# TODO (vm): Implement type stringification.

########################################################################################################################
# Type equality.                                                                                                       #
########################################################################################################################

## Checks the referential equality of the two types.
proc `===`(a: Type, b: Type): bool = cast[pointer](a) == cast[pointer](b)

proc has_equal_in(ts1: seq[Type], ts2: seq[Type]): bool
proc are_exactly_equal(ts1: seq[Type], ts2: seq[Type]): bool

proc are_equal*(t1: Type, t2: Type): bool =
  # If the two types are referentially equal, they are obviously the same!
  if t1 === t2:
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
  for i in 0 ..< ts1.len:
    if not are_equal(ts1[i], ts2[i]):
      return false
  true

########################################################################################################################
# Subtyping.                                                                                                           #
########################################################################################################################

proc sum_subtypes_sum(s1: SumType, s2: SumType): bool
proc sum_subtypes_type(s1: SumType, t2: Type): bool
proc type_subtypes_sum(t1: Type, s2: SumType): bool

proc intersection_subtypes_intersection(i1: IntersectionType, i2: IntersectionType): bool
proc intersection_subtypes_type(i1: IntersectionType, t2: Type): bool
proc type_subtypes_intersection(t1: Type, i2: IntersectionType): bool

proc tuple_subtypes_tuple(t1: TupleType, t2: TupleType): bool

proc is_subtype*(t1: Type, t2: Type): bool =
  # Because basic types are interned, this case trivially covers all basic types without subtyping interactions.
  if t1 === t2:
    return true

  case t1.kind
  of Kind.TypeVariable:
    let tv1 = cast[TypeVariable](t1)
    if is_subtype(tv1.upper_bound, t2):
      return true

  of Kind.Nothing:
    return true

  of Kind.Sum:
    let s1 = cast[SumType](t1)
    if t2.kind == Kind.Sum:
      let s2 = cast[SumType](t2)
      return sum_subtypes_sum(s1, s2)
    else:
      if sum_subtypes_type(s1, t2):
        return true

  of Kind.Intersection:
    let i1 = cast[IntersectionType](t1)
    if t2.kind == Kind.Intersection:
      let i2 = cast[IntersectionType](t2)
      return intersection_subtypes_intersection(i1, i2)
    else:
      if intersection_subtypes_type(i1, t2):
        return true

  of Kind.Tuple:
    if t2.kind == Kind.Tuple:
      return tuple_subtypes_tuple(cast[TupleType](t1), cast[TupleType](t2))

  of Kind.Function:
    if t2.kind == Kind.Function:
      let f1 = cast[FunctionType](t1)
      let f2 = cast[FunctionType](t2)
      return is_subtype(f2.input, f1.input) and is_subtype(f1.output, f2.output)

  of Kind.List:
    if t2.kind == Kind.List:
      let l1 = cast[ListType](t1)
      let l2 = cast[ListType](t2)
      return is_subtype(l1.element, l2.element)

  of Kind.Map:
    if t2.kind == Kind.Map:
      let m1 = cast[MapType](t1)
      let m2 = cast[MapType](t2)
      # TODO (vm): Variance for maps?
      return are_equal(m1.key, m2.key) and are_equal(m1.value, m2.value)

  # TODO (vm): This case can be removed if symbol types are interned.
  of Kind.Symbol:
    if t2.kind == Kind.Symbol:
      let s1 = cast[SymbolType](t1)
      let s2 = cast[SymbolType](t2)
      return s1.name == s2.name

  else: discard

  case t2.kind
  of Kind.TypeVariable:
    let tv2 = cast[TypeVariable](t2)
    is_subtype(t1, tv2.lower_bound)

  of Kind.Any:
    true

  of Kind.Sum:
    # t1 is definitely NOT a sum type, because the case SumType/SumType immediately returns in the first `case of`
    # statement above. Hence, we can safely call `type_subtypes_sum`.
    let s2 = cast[SumType](t2)
    type_subtypes_sum(t1, s2)

  of Kind.Intersection:
    # t1 is definitely NOT an intersection type, because the case IntersectionType/IntersectionType immediately returns
    # in the first `cast of` statement above. Hence, we can safely call `type_subtypes_intersection`.
    let i2 = cast[IntersectionType](t2)
    type_subtypes_intersection(t1, i2)

  else: false

proc sum_subtypes_sum(s1: SumType, s2: SumType): bool =
  for p1 in s1.parts:
    if not type_subtypes_sum(p1, s2):
      return false
  true

proc sum_subtypes_type(s1: SumType, t2: Type): bool =
  for p1 in s1.parts:
    if not is_subtype(p1, t2):
      return false
  true

proc type_subtypes_sum(t1: Type, s2: SumType): bool =
  for p2 in s2.parts:
    if is_subtype(t1, p2):
      return true
  false

proc intersection_subtypes_intersection(i1: IntersectionType, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not intersection_subtypes_type(i1, p2):
      return false
  true

proc intersection_subtypes_type(i1: IntersectionType, t2: Type): bool =
  for p1 in i1.parts:
    if is_subtype(p1, t2):
      return true
  false

proc type_subtypes_intersection(t1: Type, i2: IntersectionType): bool =
  for p2 in i2.parts:
    if not is_subtype(t1, p2):
      return false
  true

proc tuple_subtypes_tuple(t1: TupleType, t2: TupleType): bool =
  let es1 = t1.elements
  let es2 = t2.elements
  if es1.len != es2.len:
    return false

  for i in 0 ..< es1.len:
    if not is_subtype(es1[i], es2[i]):
      return false
  true

########################################################################################################################
# Type function benchmarks.                                                                                            #
########################################################################################################################

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
