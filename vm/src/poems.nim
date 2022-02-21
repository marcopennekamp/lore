import binstreams
from std/algorithm import is_sorted
import std/os
from std/sequtils import deduplicate
import std/strformat

from definitions import Function
from types import Kind, Type, Variance
from values import FunctionValueVariant

type
  Poem* = ref object
    ## A Poem is essentially a single unit of bytecode which contains type and multi-function definitions. A program
    ## may consist of many Poems. An explicit organisation of which items to put in which files is not prescribed. A
    ## Lore compiler may decide to put a whole program into a single Poem (as long as the Constants table suffices),
    ## put each function and type definition into its own Poem, or anything in between.
    ##
    ## After a Poem has been read, it contains mostly unresolved objects, such as unresolved types and functions.
    ## Resolution entails creating objects in the right order (e.g. creating types first) and then building the
    ## required structures with the correct pointers.
    ##
    ## `.poem` files must be encoded in big endian.
    constants*: PoemConstants
    schemas*: seq[PoemSchema]
    global_variables*: seq[PoemGlobalVariable]
    functions*: seq[PoemFunction]

  PoemConstants* = ref object
    types*: seq[PoemType]
    values*: seq[PoemValue]
    names*: seq[string]
    intrinsics*: seq[string]
    schemas*: seq[string]
    global_variables*: seq[string]
    multi_functions*: seq[string]
    meta_shapes*: seq[PoemMetaShape]

  PoemSchema* = ref object of RootObj
    kind*: Kind
    name*: string
    type_parameters*: seq[PoemTypeParameter]
    supertraits*: seq[PoemNamedType]

  PoemTraitSchema* = ref object of PoemSchema
    inherited_shape_type*: PoemShapeType

  PoemStructSchema* = ref object of PoemSchema
    properties*: seq[PoemStructProperty]

  PoemStructProperty* = ref object
    name*: string
    tpe*: PoemType
    is_open*: bool

  PoemGlobalVariable* = ref object of RootObj
    name*: string

  PoemEagerGlobalVariable* = ref object of PoemGlobalVariable
    value*: PoemValue

  PoemLazyGlobalVariable* = ref object of PoemGlobalVariable
    initializer_name*: string

  PoemFunction* = ref object
    name*: string
    type_parameters*: seq[PoemTypeParameter]
    input_type*: PoemType
    output_type*: PoemType
    is_abstract*: bool
    register_count*: uint16
    instructions*: seq[PoemInstruction]

    resolved_function*: Function
      ## This is used to refer to the corresponding Function object during multiple steps of universe resolution.

  PoemOperation* {.pure.} = enum
    ## Most poem instructions are encoded as size-truncated versions of their corresponding evaluator instruction,
    ## unless otherwise noted. Such operations are called "simple" poem operations. For example, `Const` is encoded as
    ## `(target_reg: uint16, val1: uint16)` with `op: uint16` being implicit. All poem operations that deviate from
    ## this norm are marked with comments.
    Assign

    Const

    # TODO (assembly): Allow IntConst to store up to 64 bits by using 4 uint16 arguments to store the integer.
    IntConst
    IntNeg
    IntAdd
    IntSub
    IntMul
    IntDiv
    IntEq
    IntLt
    IntLte
    IntToReal

    # TODO (assembly): Add RealConst, which can use 4 uint16 arguments to store the double value.
    RealNeg
    RealAdd
    RealSub
    RealMul
    RealDiv
    RealEq
    RealLt
    RealLte

    BooleanConst
    BooleanNot
    BooleanOr
    BooleanAnd

    StringOf
    StringConcat
    StringEq
    StringLt
    StringLte

    Tuple
      ## target_reg: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16
    TupleGet

    FunctionCall
      ## target_reg: uint16, function_reg: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16
    Lambda
      ## target_reg: uint16, mf: uint16, tpe: uint16, n: uint16, reg0: uint16, ..., reg_n: uint16
      ##
      ## Creates a new lambda function value from the creating function's type arguments. From the given `n` registers,
      ## a lambda function context is created, which can subsequently be queried with `LambdaLocal`. Type variables in
      ## `tpe` will be substituted.
    LambdaLocal

    List
      ## target_reg: uint16, tpe: uint16, n: uint16, element0_reg: uint16, ..., element_n_reg: uint16
      ##
      ## Creates an empty list with the given type and elements. Type variables in `tpe` will be substituted.
    ListAppend
      ## target_reg: uint16, list_reg: uint16, element_reg: uint16, tpe: uint16
      ##
      ## Appends `element` to `list`, giving it the new type `tpe`. Type variables in `tpe` will be substituted.
    ListAppendUntyped
    ListLength
    ListGet
      ## `ListGet` does not perform bounds checks, so only use this operation if bounds have been ensured beforehand.

    Shape
      ## target_reg: uint16, mtsh: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16

    SymbolEq

    Struct
      ## target_reg: uint16, sch: uint16, nt: uint8, treg0: uint16, ..., treg_nt, nv: uint8, vreg0: uint16, ...,
      ## vreg_nv: uint16
      ##
      ## Struct construction takes two variable-length argument lists: types and values, expressed via `nt` and `nv`.
    StructEq

    PropertyGet
      ## target_reg: uint16, instance_kind: uint8, instance_schema: uint16, instance_reg: uint16, nam: uint16
      ##
      ## Depending on `instance_kind`, this instruction is resolved to a different evaluator instruction:
      ##    - 0 (Any): PropertyGetNamed
      ##    - 1 (Shape): ShapePropertyGetNamed
      ##    - 2 (Trait): StructPropertyGetNamed
      ##    - 3 (Struct): StructPropertyGet
      ##
      ## This allows a compiler to generate just one instruction for getting a property, while still allowing the VM to
      ## optimize these accesses given some type information from the compiler. `Any` can always be chosen if the type
      ## of the value isn't clear at compile time.
      ##
      ## The compiler does not need to be aware of the memory layout of struct properties to take advantage of direct,
      ## index-based property accesses. To resolve this instruction to an index-based access, the VM needs to know the
      ## struct schema, which is passed via `instance_schema`. `instance_schema` exists if and only if `instance_kind`
      ## is `Struct`.

    Jump
    JumpIfFalse
    JumpIfTrue

    Intrinsic
      ## target_reg: uint16, intr: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16
    IntrinsicVoid
      ## intr: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16

    # TODO (assembly): Remember that this needs to be transformed into either `GlobalGetEager` or `GlobalGetLazy`.
    GlobalGet
      ## Whether this resolves to `GlobalGetEager` or `GlobalGetLazy` is determined by the laziness of the referenced
      ## global variable.
    GlobalSet

    Dispatch
      ## target_reg: uint16, mf: uint16, n: uint8, reg0: uint16, ..., reg_n: uint16

    # TODO (vm): Do we need ReturnUnit and Return0 in the public API? Or can we do this as an optimization?
    Return
    ReturnUnit
    Return0

    TypeArg
    TypeConst

  PoemInstruction* = ref object of RootObj
    discard

  PoemSimpleInstruction* = ref object of PoemInstruction
    ## A simple poem instruction is equal to its Instruction counterpart. The `operation` still has to be translated,
    ## however.
    operation*: PoemOperation
    arguments*: seq[uint16]

  PoemInstructionTuple* = ref object of PoemInstruction
    target_reg*: uint16
    arguments*: seq[uint16]

  PoemInstructionFunctionCall* = ref object of PoemInstruction
    target_reg*: uint16
    function_reg*: uint16
    arguments*: seq[uint16]

  PoemInstructionLambda* = ref object of PoemInstruction
    target_reg*: uint16
    mf*: uint16
    tpe*: uint16
    captured_registers*: seq[uint16]

  PoemInstructionList* = ref object of PoemInstruction
    target_reg*: uint16
    tpe*: uint16
    elements*: seq[uint16]

  PoemInstructionListAppend* = ref object of PoemInstruction
    target_reg*: uint16
    list_reg*: uint16
    element_reg*: uint16
    tpe*: uint16

  PoemInstructionShape* = ref object of PoemInstruction
    target_reg*: uint16
    meta_shape*: uint16
    arguments*: seq[uint16]

  PoemInstructionStruct* = ref object of PoemInstruction
    target_reg*: uint16
    schema*: uint16
    type_arguments*: seq[uint16]
    value_arguments*: seq[uint16]

  PoemInstructionPropertyGet* = ref object of PoemInstruction
    target_reg*: uint16
    instance_kind*: PropertyGetInstanceKind
    instance_schema*: uint16
    instance_reg*: uint16
    name*: uint16

  PropertyGetInstanceKind* {.pure.} = enum Any, Shape, Trait, Struct

  PoemInstructionIntrinsic* = ref object of PoemInstruction
    target_reg*: uint16
    intrinsic*: uint16
    arguments*: seq[uint16]

  PoemInstructionIntrinsicVoid* = ref object of PoemInstruction
    intrinsic*: uint16
    arguments*: seq[uint16]

  PoemInstructionGlobalGet* = ref object of PoemInstruction
    target_reg*: uint16
    global*: uint16

  PoemInstructionDispatch* = ref object of PoemInstruction
    target_reg*: uint16
    mf*: uint16
    arguments*: seq[uint16]

  PoemMetaShape* = ref object
    property_names*: seq[string]

  PoemTypeParameter* = ref object
    name*: string
    lower_bound*: PoemType
    upper_bound*: PoemType
    variance*: Variance

  PoemType* = ref object of RootObj
    discard

  PoemTypeVariable* = ref object of PoemType
    index*: uint8

  PoemBasicType* = ref object of PoemType
    tpe*: Type

  PoemXaryType* = ref object of PoemType
    ## This unresolved type contains any number of child types and represents sums, functions, lists, etc.
    kind*: Kind
    types*: seq[PoemType]

  PoemShapeType* = ref object of PoemType
    property_names*: seq[string]
    property_types*: seq[PoemType]

  PoemSymbolType* = ref object of PoemType
    name*: string

  PoemNamedType* = ref object of PoemType
    name*: string
    type_arguments*: seq[PoemType]

  PoemValue* = ref object of RootObj
    discard

  PoemIntValue* = ref object of PoemValue
    int*: int64

  PoemRealValue* = ref object of PoemValue
    real*: float64

  PoemBooleanValue* = ref object of PoemValue
    boolean*: bool

  PoemStringValue* = ref object of PoemValue
    string*: string

  PoemTupleValue* = ref object of PoemValue
    tpe*: PoemType
    elements*: seq[PoemValue]

  PoemFunctionValue* = ref object of PoemValue
    name*: string
    tpe*: PoemType

  PoemFixedFunctionValue* = ref object of PoemFunctionValue
    ## The desired input type that the fixed function should match.
    input_type*: PoemType

  PoemLambdaFunctionValue* = ref object of PoemFunctionValue
    ## A lambda function value is specially resolved from a multi-function that only has a single function, which is
    ## how lambdas are encoded in the VM's bytecode.
    discard

  PoemMultiFunctionValue* = ref object of PoemFunctionValue
    discard

  PoemListValue* = ref object of PoemValue
    tpe*: PoemType
    elements*: seq[PoemValue]

  PoemShapeValue* = ref object of PoemValue
    tpe*: PoemShapeType
    property_values*: seq[PoemValue]

  PoemSymbolValue* = ref object of PoemValue
    name*: string

  PoemStructValue* = ref object of PoemValue
    tpe*: PoemNamedType
    property_values*: seq[PoemValue]

let any_type*: PoemType = PoemBasicType(tpe: types.any_type)
let nothing_type*: PoemType = PoemBasicType(tpe: types.nothing_type)
let int_type*: PoemType = PoemBasicType(tpe: types.int_type)
let real_type*: PoemType = PoemBasicType(tpe: types.real_type)
let boolean_type*: PoemType = PoemBasicType(tpe: types.boolean_type)
let string_type*: PoemType = PoemBasicType(tpe: types.string_type)
let unit_type*: PoemType = PoemXaryType(kind: Kind.Tuple, types: @[])

proc struct_property*(name: string, tpe: PoemType, is_open: bool): PoemStructProperty = PoemStructProperty(name: name, tpe: tpe, is_open: is_open)

proc type_parameter*(name: string, lower_bound: PoemType, upper_bound: PoemType, variance: Variance): PoemTypeParameter =
  PoemTypeParameter(name: name, lower_bound: lower_bound, upper_bound: upper_bound, variance: variance)
proc type_parameter_upper*(name: string, upper_bound: PoemType): PoemTypeParameter = type_parameter(name, nothing_type, upper_bound, Variance.Invariant)
proc type_parameter_lower*(name: string, lower_bound: PoemType): PoemTypeParameter = type_parameter(name, lower_bound, any_type, Variance.Invariant)
proc type_parameter*(name: string): PoemTypeParameter = type_parameter_lower(name, nothing_type)

proc type_parameter_upper*(name: string, upper_bound: PoemType, variance: Variance): PoemTypeParameter =
  type_parameter(name, nothing_type, upper_bound, variance)
proc type_parameter_lower*(name: string, lower_bound: PoemType, variance: Variance): PoemTypeParameter =
  type_parameter(name, lower_bound, any_type, variance)
proc type_parameter*(name: string, variance: Variance): PoemTypeParameter =
  type_parameter_lower(name, nothing_type, variance)

proc int_value*(value: int64): PoemValue = PoemIntValue(int: value)
proc real_value*(value: float64): PoemValue = PoemRealValue(real: value)
proc boolean_value(value: bool): PoemValue = PoemBooleanValue(boolean: value)
proc string_value*(value: string): PoemValue = PoemStringValue(string: value)

proc type_variable*(index: uint8): PoemType = PoemTypeVariable(index: index)

proc sum_type*(types: open_array[PoemType]): PoemType = PoemXaryType(kind: Kind.Sum, types: @types)

proc tuple_type*(types: open_array[PoemType]): PoemType = PoemXaryType(kind: Kind.Tuple, types: @types)
proc tuple_value*(elements: seq[PoemValue], tpe: PoemType): PoemValue = PoemTupleValue(tpe: tpe, elements: elements)

proc function_type*(input: PoemType, output: PoemType): PoemType = PoemXaryType(kind: Kind.Function, types: @[input, output])
proc multi_function_value*(name: string, tpe: PoemType): PoemValue = PoemMultiFunctionValue(name: name, tpe: tpe)
proc fixed_function_value*(name: string, input_type: PoemType, tpe: PoemType): PoemValue = PoemFixedFunctionValue(name: name, input_type: input_type, tpe: tpe)
proc lambda_function_value*(name: string, tpe: PoemType): PoemValue = PoemLambdaFunctionValue(name: name, tpe: tpe)

proc list_type*(element_type: PoemType): PoemType = PoemXaryType(kind: Kind.List, types: @[element_type])
proc list_value*(elements: seq[PoemValue], tpe: PoemType): PoemValue = PoemListValue(tpe: tpe, elements: elements)

proc shape_type_concrete*(property_names: seq[string], property_types: seq[PoemType]): PoemShapeType = PoemShapeType(property_names: property_names, property_types: property_types)
proc shape_type*(property_names: seq[string], property_types: seq[PoemType]): PoemType = shape_type_concrete(property_names, property_types)
proc shape_value*(tpe: PoemShapeType, property_values: seq[PoemValue]): PoemValue = PoemShapeValue(tpe: tpe, property_values: property_values)
proc shape_value_cast_type*(tpe: PoemType, property_values: seq[PoemValue]): PoemValue = PoemShapeValue(tpe: cast[PoemShapeType](tpe), property_values: property_values)
proc shape_value*(property_names: seq[string], property_types: seq[PoemType], property_values: seq[PoemValue]): PoemValue =
  shape_value(shape_type_concrete(property_names, property_types), property_values)

let empty_shape_type*: PoemShapeType = shape_type_concrete(@[], @[])

proc symbol_type*(name: string): PoemType = PoemSymbolType(name: name)
proc symbol_value*(name: string): PoemValue = PoemSymbolValue(name: name)

proc named_type_concrete*(name: string, type_arguments: seq[PoemType]): PoemNamedType = PoemNamedType(name: name, type_arguments: type_arguments)
proc named_type_concrete*(name: string): PoemNamedType = named_type_concrete(name, @[])

proc named_type*(name: string, type_arguments: seq[PoemType]): PoemType = named_type_concrete(name, type_arguments)
proc named_type*(name: string): PoemType = named_type(name, @[])

proc struct_value*(tpe: PoemNamedType, property_values: seq[PoemValue]): PoemValue =
  PoemStructValue(tpe: tpe, property_values: property_values)

proc struct_value*(name: string, type_arguments: seq[PoemType], property_values: seq[PoemValue]): PoemValue =
  let poem_type = named_type_concrete(name, type_arguments)
  struct_value(poem_type, property_values)

proc inst*(operation: PoemOperation, arguments: varargs[uint16]): PoemInstruction =
  PoemSimpleInstruction(operation: operation, arguments: @arguments)

proc inst_tuple*(target: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionTuple(target_reg: target, arguments: @arguments)

proc inst_function_call*(target: uint16, function: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionFunctionCall(target_reg: target, function_reg: function, arguments: @arguments)

proc inst_lambda*(target: uint16, mf: uint16, tpe: uint16, captured_registers: varargs[uint16]): PoemInstruction =
  PoemInstructionLambda(target_reg: target, mf: mf, tpe: tpe, captured_registers: @captured_registers)

proc inst_list*(target: uint16, tpe: uint16, elements: varargs[uint16]): PoemInstruction =
  PoemInstructionList(target_reg: target, tpe: tpe, elements: @elements)

proc inst_list_append*(target: uint16, list: uint16, element: uint16, tpe: uint16): PoemInstruction =
  PoemInstructionListAppend(target_reg: target, list_reg: list, element_reg: element, tpe: tpe)

proc inst_shape*(target: uint16, meta_shape: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionShape(target_reg: target, meta_shape: meta_shape, arguments: @arguments)

proc inst_struct*(target: uint16, schema: uint16, type_arguments: open_array[uint16], value_arguments: open_array[uint16]): PoemInstruction =
  PoemInstructionStruct(target_reg: target, schema: schema, type_arguments: @type_arguments, value_arguments: @value_arguments)

proc inst_any_property_get*(target: uint16, instance: uint16, name: uint16): PoemInstruction =
  PoemInstructionPropertyGet(target_reg: target, instance_kind: PropertyGetInstanceKind.Any, instance_reg: instance, name: name)

proc inst_shape_property_get*(target: uint16, instance: uint16, name: uint16): PoemInstruction =
  PoemInstructionPropertyGet(target_reg: target, instance_kind: PropertyGetInstanceKind.Shape, instance_reg: instance, name: name)

proc inst_trait_property_get*(target: uint16, instance: uint16, name: uint16): PoemInstruction =
  PoemInstructionPropertyGet(target_reg: target, instance_kind: PropertyGetInstanceKind.Trait, instance_reg: instance, name: name)

proc inst_struct_property_get*(target: uint16, instance: uint16, schema: uint16, name: uint16): PoemInstruction =
  PoemInstructionPropertyGet(target_reg: target, instance_kind: PropertyGetInstanceKind.Struct, instance_schema: schema, instance_reg: instance, name: name)

proc inst_intrinsic*(target: uint16, intrinsic: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionIntrinsic(target_reg: target, intrinsic: intrinsic, arguments: @arguments)

proc inst_intrinsic_void*(intrinsic: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionIntrinsicVoid(intrinsic: intrinsic, arguments: @arguments)

proc inst_global_get*(target: uint16, global: uint16): PoemInstruction =
  PoemInstructionGlobalGet(target_reg: target, global: global)

proc inst_dispatch*(target: uint16, mf: uint16, arguments: varargs[uint16]): PoemInstruction =
  PoemInstructionDispatch(target_reg: target, mf: mf, arguments: @arguments)

const
  tkMetadataKinded = 0'u8
  tkSum = 1'u8
  tkIntersection = 2'u8
  tkTuple = 3'u8
  tkNamed = 4'u8

  mkAny = 0'u8
  mkNothing = 1'u8
  mkInt = 2'u8
  mkReal = 3'u8
  mkBoolean = 4'u8
  mkString = 5'u8
  mkVariable = 16'u8
  mkFunction = 17'u8
  mkList = 18'u8
  mkMap = 19'u8
  mkShape = 20'u8
  mkSymbol = 21'u8

proc fail(message: string) {.noreturn.} = raise new_exception(IOError, message)

########################################################################################################################
# Read and write helpers.                                                                                              #
########################################################################################################################

template read_many(stream: FileStream, result_type: untyped, count: uint, read_one): untyped =
  var results = new_seq_of_cap[result_type](count)
  var i: uint = 0
  while i < count:
    results.add(read_one(stream))
    i += 1
  results

template read_many_with_count(stream: FileStream, result_type: untyped, count_type, read_one): untyped =
  let count = stream.read(count_type)
  read_many(stream, result_type, count, read_one)

template write_many(stream: FileStream, items, write_one): untyped =
  for item in items:
    write_one(stream, item)

template write_many_with_count(stream: FileStream, items, count_type, write_one): untyped =
  let count = count_type(items.len)
  if count > high(count_type):
    raise new_exception(IOError, "Cannot write {count} elements: The count type is too small.")
  stream.write(count)
  stream.write_many(items, write_one)

proc read_uint16(stream: FileStream): uint16 = stream.read(uint16)
proc write_uint16(stream: FileStream, value: uint16) = stream.write(value)

########################################################################################################################
# Poems (top-level).                                                                                                   #
########################################################################################################################

proc read_constants(stream: FileStream): PoemConstants
proc read_schema(stream: FileStream): PoemSchema
proc read_struct_property(stream: FileStream): PoemStructProperty
proc read_global_variable(stream: FileStream): PoemGlobalVariable
proc read_function(stream: FileStream): PoemFunction
proc read_instruction(stream: FileStream): PoemInstruction
proc read_instruction_arguments_with_count(stream: FileStream): seq[uint16]
proc read_meta_shape(stream: FileStream): PoemMetaShape
proc read_type_parameters(stream: FileStream): seq[PoemTypeParameter]
proc read_type(stream: FileStream): PoemType
proc read_value(stream: FileStream): PoemValue
proc read_string_with_length(stream: FileStream): string

proc write_constants(stream: FileStream, constants: PoemConstants)
proc write_schema(stream: FileStream, schema: PoemSchema)
proc write_struct_property(stream: FileStream, property: PoemStructProperty)
proc write_global_variable(stream: FileStream, global_variable: PoemGlobalVariable)
proc write_function(stream: FileStream, function: PoemFunction)
proc write_instruction(stream: FileStream, instruction: PoemInstruction)
proc write_operation(stream: FileStream, operation: PoemOperation)
proc write_instruction_arguments_with_count(stream: FileStream, arguments: seq[uint16])
proc write_meta_shape(stream: FileStream, meta_shape: PoemMetaShape)
proc write_shape_property_names(stream: FileStream, property_names: seq[string], with_count: bool)
proc write_type_parameters(stream: FileStream, type_parameters: seq[PoemTypeParameter])
proc write_type(stream: FileStream, tpe: PoemType)
proc write_value(stream: FileStream, value: PoemValue)
proc write_string_with_length(stream: FileStream, string: string)

method write(instruction: PoemInstruction, stream: FileStream) {.base, locks: "unknown".}
method write(global_variable: PoemGlobalVariable, stream: FileStream) {.base, locks: "unknown".}
method write(tpe: PoemType, stream: FileStream) {.base, locks: "unknown".}
method write(value: PoemValue, stream: FileStream) {.base, locks: "unknown".}

proc read*(path: string): Poem =
  if unlikely(not file_exists(path)):
    fail(fmt"""Poem file "{path}" does not exist.""")

  let stream = new_file_stream(path, big_endian, fmRead)
  defer: stream.close()

  # Check that the file starts with "poem".
  let magic_string = stream.read_str(4)
  if magic_string != "poem":
    fail(fmt"""Poem file "{path}" has an illegal file header. The file must begin with the ASCII string `poem`.""")

  let constants = stream.read_constants()
  let schemas = stream.read_many_with_count(PoemSchema, uint16, read_schema)
  let global_variables = stream.read_many_with_count(PoemGlobalVariable, uint16, read_global_variable)
  let functions = stream.read_many_with_count(PoemFunction, uint16, read_function)
  Poem(
    constants: constants,
    schemas: schemas,
    global_variables: global_variables,
    functions: functions,
  )

proc write*(path: string, poem: Poem) =
  let stream = new_file_stream(path, big_endian, fmWrite)
  defer: stream.close()

  stream.write_str("poem")
  stream.write_constants(poem.constants)
  stream.write_many_with_count(poem.schemas, uint16, write_schema)
  stream.write_many_with_count(poem.global_variables, uint16, write_global_variable)
  stream.write_many_with_count(poem.functions, uint16, write_function)

########################################################################################################################
# Constants.                                                                                                           #
########################################################################################################################

proc read_constants(stream: FileStream): PoemConstants =
  let types = stream.read_many_with_count(PoemType, uint16, read_type)
  let values = stream.read_many_with_count(PoemValue, uint16, read_value)
  let names = stream.read_many_with_count(string, uint16, read_string_with_length)
  let intrinsics = stream.read_many_with_count(string, uint16, read_string_with_length)
  let schemas = stream.read_many_with_count(string, uint16, read_string_with_length)
  let global_variables = stream.read_many_with_count(string, uint16, read_string_with_length)
  let multi_functions = stream.read_many_with_count(string, uint16, read_string_with_length)
  let meta_shapes = stream.read_many_with_count(PoemMetaShape, uint16, read_meta_shape)

  PoemConstants(
    types: types,
    values: values,
    names: names,
    intrinsics: intrinsics,
    schemas: schemas,
    global_variables: global_variables,
    multi_functions: multi_functions,
    meta_shapes: meta_shapes,
  )

proc write_constants(stream: FileStream, constants: PoemConstants) =
  stream.write_many_with_count(constants.types, uint16, write_type)
  stream.write_many_with_count(constants.values, uint16, write_value)
  stream.write_many_with_count(constants.names, uint16, write_string_with_length)
  stream.write_many_with_count(constants.intrinsics, uint16, write_string_with_length)
  stream.write_many_with_count(constants.schemas, uint16, write_string_with_length)
  stream.write_many_with_count(constants.global_variables, uint16, write_string_with_length)
  stream.write_many_with_count(constants.multi_functions, uint16, write_string_with_length)
  stream.write_many_with_count(constants.meta_shapes, uint16, write_meta_shape)

########################################################################################################################
# Schemas.                                                                                                             #
########################################################################################################################

proc read_schema(stream: FileStream): PoemSchema =
  let kind_code = stream.read(uint8)
  let kind =
    case kind_code
    of 0: Kind.Trait
    of 1: Kind.Struct
    else: raise new_exception(IOError, fmt"Unknown schema kind {kind_code}.")
  let name = stream.read_string_with_length()
  let type_parameters = stream.read_type_parameters()
  let supertraits = stream.read_many_with_count(PoemType, uint8, read_type)
  for supertrait in supertraits:
    if not (supertrait of PoemNamedType):
      raise new_exception(IOError, fmt"The schema {name} has supertraits which are not NamedTypes. All supertraits must be NamedTypes.")

  let schema =
    if kind == Kind.Trait:
      let inherited_shape_type = stream.read_type()
      if not (inherited_shape_type of PoemShapeType):
        raise new_exception(IOError, fmt"The trait schema {name} has an inherited shape type which is not a ShapeType.")
      PoemTraitSchema(inherited_shape_type: cast[PoemShapeType](inherited_shape_type))
    elif kind == Kind.Struct:
      let properties = stream.read_many_with_count(PoemStructProperty, uint16, read_struct_property)
      PoemStructSchema(properties: properties)
    else:
      raise new_exception(IOError, fmt"The schema kind {kind} is invalid.")

  schema.kind = kind
  schema.name = name
  schema.type_parameters = type_parameters
  schema.supertraits = cast[seq[PoemNamedType]](supertraits)
  schema

proc read_struct_property(stream: FileStream): PoemStructProperty =
  let name = stream.read_string_with_length()
  let tpe = stream.read_type()
  let is_open = stream.read(bool)
  PoemStructProperty(name: name, tpe: tpe, is_open: is_open)

proc write_schema(stream: FileStream, schema: PoemSchema) =
  let kind_code: uint8 =
    case schema.kind
    of Kind.Trait: 0'u8
    of Kind.Struct: 1'u8
    else: raise new_exception(IOError, fmt"Invalid schema kind {schema.kind}.")
  stream.write(kind_code)
  stream.write_string_with_length(schema.name)
  stream.write_type_parameters(schema.type_parameters)
  stream.write_many_with_count(schema.supertraits, uint8, write_type)

  if schema of PoemTraitSchema:
    let schema = cast[PoemTraitSchema](schema)
    stream.write_type(schema.inherited_shape_type)
  elif schema of PoemStructSchema:
    let schema = cast[PoemStructSchema](schema)
    stream.write_many_with_count(schema.properties, uint16, write_struct_property)
  else:
    raise new_exception(IOError, fmt"The schema type is invalid.")

proc write_struct_property(stream: FileStream, property: PoemStructProperty) =
  stream.write_string_with_length(property.name)
  stream.write_type(property.tpe)
  stream.write(property.is_open)

########################################################################################################################
# Global variables.                                                                                                    #
########################################################################################################################

proc read_global_variable(stream: FileStream): PoemGlobalVariable =
  let name = stream.read_string_with_length()
  let is_lazy = stream.read(bool)
  if not is_lazy:
    let value = stream.read_value()
    PoemEagerGlobalVariable(name: name, value: value)
  else:
    let initializer_name = stream.read_string_with_length()
    PoemLazyGlobalVariable(name: name, initializer_name: initializer_name)

proc write_global_variable(stream: FileStream, global_variable: PoemGlobalVariable) =
  global_variable.write(stream)

method write(global_variable: PoemGlobalVariable, stream: FileStream) {.base, locks: "unknown".} =
  quit("Please implement `write` for all PoemGlobalVariables.")

method write(global_variable: PoemEagerGlobalVariable, stream: FileStream) {.locks: "unknown".} =
  stream.write_string_with_length(global_variable.name)
  stream.write(false)
  stream.write_value(global_variable.value)

method write(global_variable: PoemLazyGlobalVariable, stream: FileStream) {.locks: "unknown".} =
  stream.write_string_with_length(global_variable.name)
  stream.write(true)
  stream.write_string_with_length(global_variable.initializer_name)

########################################################################################################################
# Functions.                                                                                                           #
########################################################################################################################

proc read_function(stream: FileStream): PoemFunction =
  let name = stream.read_string_with_length()
  let type_parameters = stream.read_type_parameters()
  let input_type = stream.read_type()
  let output_type = stream.read_type()
  let is_abstract = stream.read(bool)

  var function = PoemFunction(
    name: name,
    type_parameters: type_parameters,
    input_type: input_type,
    output_type: output_type,
    is_abstract: is_abstract,
  )

  if not is_abstract:
    function.register_count = stream.read(uint16)
    function.instructions = stream.read_many_with_count(PoemInstruction, uint16, read_instruction)

  function

proc write_function(stream: FileStream, function: PoemFunction) =
  stream.write_string_with_length(function.name)
  stream.write_type_parameters(function.type_parameters)
  stream.write_type(function.input_type)
  stream.write_type(function.output_type)
  stream.write(function.is_abstract)

  if not function.is_abstract:
    stream.write(function.register_count)
    stream.write_many_with_count(function.instructions, uint16, write_instruction)

########################################################################################################################
# Instructions.                                                                                                        #
########################################################################################################################

proc simple_argument_count(operation: PoemOperation): uint8

proc read_instruction(stream: FileStream): PoemInstruction =
  let operation = cast[PoemOperation](stream.read(uint16))
  case operation
  of PoemOperation.Tuple:
    PoemInstructionTuple(
      target_reg: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  of FunctionCall:
    PoemInstructionFunctionCall(
      target_reg: stream.read(uint16),
      function_reg: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  of PoemOperation.Lambda:
    PoemInstructionLambda(
      target_reg: stream.read(uint16),
      mf: stream.read(uint16),
      tpe: stream.read(uint16),
      captured_registers: stream.read_many_with_count(uint16, uint16, read_uint16),
    )

  of PoemOperation.List:
    PoemInstructionList(
      target_reg: stream.read(uint16),
      tpe: stream.read(uint16),
      elements: stream.read_many_with_count(uint16, uint16, read_uint16),
    )

  of PoemOperation.ListAppend:
    PoemInstructionListAppend(
      target_reg: stream.read(uint16),
      list_reg: stream.read(uint16),
      element_reg: stream.read(uint16),
      tpe: stream.read(uint16),
    )

  of PoemOperation.Shape:
    PoemInstructionShape(
      target_reg: stream.read(uint16),
      meta_shape: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  of PoemOperation.Struct:
    PoemInstructionStruct(
      target_reg: stream.read(uint16),
      schema: stream.read(uint16),
      type_arguments: stream.read_instruction_arguments_with_count(),
      value_arguments: stream.read_instruction_arguments_with_count(),
    )

  of PropertyGet:
    let target_reg = stream.read(uint16)
    let instance_kind = cast[PropertyGetInstanceKind](stream.read(uint8))
    let instance_schema =
      if instance_kind == PropertyGetInstanceKind.Struct: stream.read(uint16)
      else: 0'u16
    let instance_reg = stream.read(uint16)
    let name = stream.read(uint16)

    PoemInstructionPropertyGet(
      target_reg: target_reg,
      instance_kind: instance_kind,
      instance_schema: instance_schema,
      instance_reg: instance_reg,
      name: name,
    )

  of Intrinsic:
    PoemInstructionIntrinsic(
      target_reg: stream.read(uint16),
      intrinsic: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  of IntrinsicVoid:
    PoemInstructionIntrinsicVoid(
      intrinsic: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  of GlobalGet:
    PoemInstructionGlobalGet(
      target_reg: stream.read(uint16),
      global: stream.read(uint16),
    )

  of Dispatch:
    PoemInstructionDispatch(
      target_reg: stream.read(uint16),
      mf: stream.read(uint16),
      arguments: stream.read_instruction_arguments_with_count(),
    )

  else:
    let argument_count = operation.simple_argument_count
    let arguments = stream.read_many(uint16, argument_count, read_uint16)
    PoemSimpleInstruction(operation: operation, arguments: arguments)

proc read_instruction_arguments_with_count(stream: FileStream): seq[uint16] =
  stream.read_many_with_count(uint16, uint8, read_uint16)

proc write_instruction(stream: FileStream, instruction: PoemInstruction) =
  instruction.write(stream)

proc write_operation(stream: FileStream, operation: PoemOperation) =
  stream.write(cast[uint16](operation))

proc write_instruction_arguments_with_count(stream: FileStream, arguments: seq[uint16]) =
  stream.write_many_with_count(arguments, uint8, write_uint16)

method write(instruction: PoemInstruction, stream: FileStream) {.base, locks: "unknown".} =
  quit("Please implement `write` for all PoemInstructions")

method write(instruction: PoemSimpleInstruction, stream: FileStream) {.locks: "unknown".} =
  if int(instruction.operation.simple_argument_count) != instruction.arguments.len:
    quit(fmt"The simple poem instruction's argument length must be equal to the expected argument length. Operation: {instruction.operation}.")

  stream.write_operation(instruction.operation)
  stream.write_many(instruction.arguments, write_uint16)
  
method write(instruction: PoemInstructionTuple, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Tuple)
  stream.write(instruction.target_reg)
  stream.write_instruction_arguments_with_count(instruction.arguments)

method write(instruction: PoemInstructionFunctionCall, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.FunctionCall)
  stream.write(instruction.target_reg)
  stream.write(instruction.function_reg)
  stream.write_instruction_arguments_with_count(instruction.arguments)

method write(instruction: PoemInstructionLambda, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Lambda)
  stream.write(instruction.target_reg)
  stream.write(instruction.mf)
  stream.write(instruction.tpe)
  stream.write_many_with_count(instruction.captured_registers, uint16, write_uint16)

method write(instruction: PoemInstructionList, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.List)
  stream.write(instruction.target_reg)
  stream.write(instruction.tpe)
  stream.write_many_with_count(instruction.elements, uint16, write_uint16)

method write(instruction: PoemInstructionListAppend, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.ListAppend)
  stream.write(instruction.target_reg)
  stream.write(instruction.list_reg)
  stream.write(instruction.element_reg)
  stream.write(instruction.tpe)

method write(instruction: PoemInstructionShape, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Shape)
  stream.write(instruction.target_reg)
  stream.write(instruction.meta_shape)
  stream.write_instruction_arguments_with_count(instruction.arguments)

method write(instruction: PoemInstructionStruct, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Struct)
  stream.write(instruction.target_reg)
  stream.write(instruction.schema)
  stream.write_instruction_arguments_with_count(instruction.type_arguments)
  stream.write_instruction_arguments_with_count(instruction.value_arguments)

method write(instruction: PoemInstructionPropertyGet, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.PropertyGet)
  stream.write(instruction.target_reg)
  stream.write(cast[uint8](instruction.instance_kind))
  if instruction.instance_kind == PropertyGetInstanceKind.Struct:
    stream.write(instruction.instance_schema)
  stream.write(instruction.instance_reg)
  stream.write(instruction.name)

method write(instruction: PoemInstructionIntrinsic, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Intrinsic)
  stream.write(instruction.target_reg)
  stream.write(instruction.intrinsic)
  stream.write_instruction_arguments_with_count(instruction.arguments)

method write(instruction: PoemInstructionIntrinsicVoid, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.IntrinsicVoid)
  stream.write(instruction.intrinsic)
  stream.write_instruction_arguments_with_count(instruction.arguments)

method write(instruction: PoemInstructionGlobalGet, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.GlobalGet)
  stream.write(instruction.target_reg)
  stream.write(instruction.global)

method write(instruction: PoemInstructionDispatch, stream: FileStream) {.locks: "unknown".} =
  stream.write_operation(PoemOperation.Dispatch)
  stream.write(instruction.target_reg)
  stream.write(instruction.mf)
  stream.write_instruction_arguments_with_count(instruction.arguments)

proc simple_argument_count(operation: PoemOperation): uint8 =
  ## Returns the argument length of `operation`, provided that it is a simple operation.
  case operation
  of ReturnUnit, Return0: 0
  of Jump, Return: 1
  of Assign, Const, IntConst, IntNeg, IntToReal, RealNeg, BooleanConst, BooleanNot, StringOf, LambdaLocal, ListLength,
     JumpIfFalse, JumpIfTrue, GlobalSet, TypeArg, TypeConst: 2
  of IntAdd, IntSub, IntMul, IntDiv, IntEq, IntLt, IntLte, RealAdd, RealSub, RealMul, RealDiv, RealEq, RealLt, RealLte,
     BooleanOr, BooleanAnd, StringConcat, StringEq, StringLt, StringLte, TupleGet, ListAppendUntyped, ListGet,
     SymbolEq, StructEq: 3
  of PoemOperation.Tuple, FunctionCall, PoemOperation.Lambda, PoemOperation.Shape, PoemOperation.List, ListAppend,
     PoemOperation.Struct, PropertyGet, Intrinsic, IntrinsicVoid, GlobalGet, Dispatch:
    quit(fmt"Poem operation {operation} is not simple!")

########################################################################################################################
# Meta shapes.                                                                                                         #
########################################################################################################################

proc read_meta_shape(stream: FileStream): PoemMetaShape =
  let property_names = stream.read_many_with_count(string, uint8, read_string_with_length)
  PoemMetaShape(property_names: property_names)

proc write_meta_shape(stream: FileStream, meta_shape: PoemMetaShape) =
  stream.write_shape_property_names(meta_shape.property_names, with_count = true)

proc write_shape_property_names(stream: FileStream, property_names: seq[string], with_count: bool) =
  if not property_names.is_sorted():
    fail(fmt"The property names {property_names} of a shape must be sorted lexicographically.")

  let unique_names = property_names.deduplicate(is_sorted = true)
  if unique_names != property_names:
    fail(fmt"The property names {property_names} of a shape may not contain duplicates.")

  if with_count:
    stream.write_many_with_count(property_names, uint8, write_string_with_length)
  else:
    stream.write_many(property_names, write_string_with_length)

########################################################################################################################
# Type parameters.                                                                                                     #
########################################################################################################################

proc ensure_type_parameter_count(type_parameter_count: int) =
  if type_parameter_count > types.max_type_parameters:
    fail(fmt"A schema or function has {type_parameter_count} type parameters, but the maximum is {types.max_type_parameters}.")

proc read_type_parameter(stream: FileStream): PoemTypeParameter =
  let name = stream.read_string_with_length()
  let lower_bound = stream.read_type()
  let upper_bound = stream.read_type()
  let variance = Variance(stream.read(uint8))

  PoemTypeParameter(
    name: name,
    lower_bound: lower_bound,
    upper_bound: upper_bound,
    variance: variance,
  )

proc read_type_parameters(stream: FileStream): seq[PoemTypeParameter] =
  let type_parameters = stream.read_many_with_count(PoemTypeParameter, uint8, read_type_parameter)
  ensure_type_parameter_count(type_parameters.len)
  type_parameters

proc write_type_parameter(stream: FileStream, parameter: PoemTypeParameter) =
  stream.write_string_with_length(parameter.name)
  stream.write_type(parameter.lower_bound)
  stream.write_type(parameter.upper_bound)
  stream.write(uint8(ord(parameter.variance)))

proc write_type_parameters(stream: FileStream, type_parameters: seq[PoemTypeParameter]) =
  ensure_type_parameter_count(type_parameters.len)
  stream.write_many_with_count(type_parameters, uint8, write_type_parameter)

########################################################################################################################
# Types.                                                                                                               #
########################################################################################################################

template read_xary_type(stream: FileStream, xary_kind: Kind, metadata: uint8): untyped =
  let types = stream.read_many(PoemType, metadata, read_type)
  PoemXaryType(kind: xary_kind, types: types)

proc read_type(stream: FileStream): PoemType =
  let tag = stream.read(uint8)
  let kind = tag shr 5
  let metadata = tag and 0b11111

  case kind
  of tkMetadataKinded:
    case metadata
    of mkAny: PoemBasicType(tpe: types.any_type)
    of mkNothing: PoemBasicType(tpe: types.nothing_type)
    of mkInt: PoemBasicType(tpe: types.int_type)
    of mkReal: PoemBasicType(tpe: types.real_type)
    of mkBoolean: PoemBasicType(tpe: types.boolean_type)
    of mkString: PoemBasicType(tpe: types.string_type)
    of mkVariable:
      let index = stream.read(uint8)
      PoemTypeVariable(index: index)
    of mkFunction:
      let input = stream.read_type()
      let output = stream.read_type()
      PoemXaryType(kind: Kind.Function, types: @[input, output])
    of mkList:
      let element = stream.read_type()
      PoemXaryType(kind: Kind.List, types: @[element])
    of mkMap:
      let key = stream.read_type()
      let value = stream.read_type()
      PoemXaryType(kind: Kind.Map, types: @[key, value])
    of mkShape:
      let property_count = stream.read(uint8)
      let property_names = stream.read_many(string, property_count, read_string_with_length)
      let property_types = stream.read_many(PoemType, property_count, read_type)
      PoemShapeType(property_names: property_names, property_types: property_types)
    of mkSymbol:
      let name = stream.read_string_with_length()
      PoemSymbolType(name: name)
    else: raise new_exception(IOError, fmt"Unknown metadata-kinded type {metadata}.")

  of tkSum: stream.read_xary_type(Kind.Sum, metadata)
  of tkIntersection: stream.read_xary_type(Kind.Intersection, metadata)
  of tkTuple: stream.read_xary_type(Kind.Tuple, metadata)

  of tkNamed:
    let name = stream.read_string_with_length()
    let type_arguments = stream.read_many(PoemType, metadata, read_type)
    PoemNamedType(name: name, type_arguments: type_arguments)

  else: raise new_exception(IOError, fmt"Unknown type kind {kind}.")

proc write_type(stream: FileStream, tpe: PoemType) =
  tpe.write(stream)

proc write_type_tag(stream: FileStream, tag_kind: uint8, tag_metadata: uint8) =
  let tag: uint8 = (tag_kind shl 5) or tag_metadata
  stream.write(tag)

proc write_type_tag(stream: FileStream, kind: Kind, child_count: uint8) =
  let (tag_kind, tag_metadata) = case kind
  of Kind.TypeVariable: (tkMetadataKinded, mkVariable)
  of Kind.Any: (tkMetadataKinded, mkAny)
  of Kind.Nothing: (tkMetadataKinded, mkNothing)
  of Kind.Int: (tkMetadataKinded, mkInt)
  of Kind.Real: (tkMetadataKinded, mkReal)
  of Kind.Boolean: (tkMetadataKinded, mkBoolean)
  of Kind.String: (tkMetadataKinded, mkString)
  of Kind.Sum: (tkSum, child_count)
  of Kind.Intersection: (tkIntersection, child_count)
  of Kind.Tuple: (tkTuple, child_count)
  of Kind.Function: (tkMetadataKinded, mkFunction)
  of Kind.List: (tkMetadataKinded, mkList)
  of Kind.Map: (tkMetadataKinded, mkMap)
  of Kind.Shape: (tkMetadataKinded, mkShape)
  of Kind.Symbol: (tkMetadataKinded, mkSymbol)
  of Kind.Trait: (tkNamed, child_count)
  of Kind.Struct: (tkNamed, child_count)
  stream.write_type_tag(tag_kind, tag_metadata)

proc write_type_tag(stream: FileStream, kind: Kind) =
  stream.write_type_tag(kind, 0)

proc length_to_tag_metadata(length: int): uint8 =
  if unlikely(length > 31):
    fail(fmt"Types cannot have more than 31 type children.")
  return cast[uint8](length)

method write(tpe: PoemType, stream: FileStream) {.base, locks: "unknown".} =
  quit("Please implement `write` for all PoemTypes.")

method write(tpe: PoemTypeVariable, stream: FileStream) {.locks: "unknown".} =
  stream.write_type_tag(Kind.TypeVariable)
  stream.write(tpe.index)

method write(tpe: PoemBasicType, stream: FileStream) {.locks: "unknown".} =
  stream.write_type_tag(tpe.tpe.kind)

method write(tpe: PoemXaryType, stream: FileStream) =
  let child_count = uint64(tpe.types.len)
  if child_count > 31:
    fail(fmt"Types of kind {tpe.kind} cannot have more than 31 type children.")

  stream.write_type_tag(tpe.kind, length_to_tag_metadata(tpe.types.len))
  stream.write_many(tpe.types, write_type)

method write(tpe: PoemShapeType, stream: FileStream) =
  let property_count = uint64(tpe.property_names.len)
  if property_count > 255:
    fail(fmt"Shape types cannot have more than 255 properties.")

  stream.write_type_tag(Kind.Shape)
  stream.write(cast[uint8](property_count))
  stream.write_shape_property_names(tpe.property_names, with_count = false)
  stream.write_many(tpe.property_types, write_type)

method write(tpe: PoemSymbolType, stream: FileStream) {.locks: "unknown".} =
  stream.write_type_tag(Kind.Symbol)
  stream.write_string_with_length(tpe.name)

method write(tpe: PoemNamedType, stream: FileStream) =
  stream.write_type_tag(tkNamed, length_to_tag_metadata(tpe.type_arguments.len))
  stream.write_string_with_length(tpe.name)
  stream.write_many(tpe.type_arguments, write_type)

########################################################################################################################
# Values.                                                                                                              #
########################################################################################################################

proc read_value(stream: FileStream): PoemValue =
  let tpe = stream.read_type()
  if tpe of PoemBasicType:
    case cast[PoemBasicType](tpe).tpe.kind
    of Kind.Int:
      let value = stream.read(int64)
      int_value(value)
    of Kind.Real:
      let value = stream.read(float64)
      real_value(value)
    of Kind.Boolean:
      let value = stream.read(bool)
      boolean_value(value)
    of Kind.String:
      let value = stream.read_string_with_length()
      string_value(value)
    else: fail("Invalid kind for basic type value.")
  elif tpe of PoemXaryType:
    let xary = cast[PoemXaryType](tpe)
    case xary.kind
    of Kind.Tuple:
      let elements = stream.read_many(PoemValue, cast[uint](xary.types.len), read_value)
      tuple_value(elements, tpe)
    of Kind.Function:
      let variant = cast[FunctionValueVariant](stream.read(uint8))
      let name = stream.read_string_with_length()
      case variant
      of FunctionValueVariant.Multi:
        multi_function_value(name, tpe)
      of FunctionValueVariant.Fixed:
        let input_type = stream.read_type()
        fixed_function_value(name, input_type, tpe)
      of FunctionValueVariant.Lambda:
        lambda_function_value(name, tpe)
    of Kind.List:
      let elements = stream.read_many_with_count(PoemValue, uint16, read_value)
      list_value(elements, tpe)
    else:
      fail("Only tuple, function, and list values are supported for now.")
  elif tpe of PoemShapeType:
    let shape_type = cast[PoemShapeType](tpe)
    let property_values = stream.read_many(PoemValue, uint(shape_type.property_names.len), read_value)
    shape_value(shape_type, property_values)
  elif tpe of PoemSymbolType:
    let symbol_type = cast[PoemSymbolType](tpe)
    symbol_value(symbol_type.name)
  elif tpe of PoemNamedType:
    let named_type = cast[PoemNamedType](tpe)
    let property_values = stream.read_many_with_count(PoemValue, uint16, read_value)
    struct_value(named_type, property_values)
  else:
    fail(fmt"Unknown poem type.")

proc write_value(stream: FileStream, value: PoemValue) =
  value.write(stream)

method write(value: PoemValue, stream: FileStream) {.base, locks: "unknown".} =
  quit("Please implement `write` for all PoemValues")

method write(value: PoemIntValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(int_type)
  stream.write(value.int)

method write(value: PoemRealValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(real_type)
  stream.write(value.real)

method write(value: PoemBooleanValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(boolean_type)
  stream.write(value.boolean)

method write(value: PoemStringValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(string_type)
  stream.write_string_with_length(value.string)

method write(value: PoemTupleValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(value.tpe)
  stream.write_many(value.elements, write_value)

method write(value: PoemFunctionValue, stream: FileStream) {.locks: "unknown".} =
  quit("Please implement `write` for all PoemFunctionValues")

proc write_function_value_commons(stream: FileStream, value: PoemFunctionValue, variant: FunctionValueVariant) =
  stream.write_type(value.tpe)
  stream.write(cast[uint8](variant))
  stream.write_string_with_length(value.name)

method write(value: PoemMultiFunctionValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_function_value_commons(value, FunctionValueVariant.Multi)

method write(value: PoemFixedFunctionValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_function_value_commons(value, FunctionValueVariant.Fixed)
  stream.write_type(value.input_type)

method write(value: PoemLambdaFunctionValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_function_value_commons(value, FunctionValueVariant.Lambda)

method write(value: PoemListValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(value.tpe)
  stream.write_many_with_count(value.elements, uint16, write_value)

method write(value: PoemShapeValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(value.tpe)
  stream.write_many(value.property_values, write_value)

method write(value: PoemSymbolValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(symbol_type(value.name))

method write(value: PoemStructValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(value.tpe)
  stream.write_many_with_count(value.property_values, uint16, write_value)

########################################################################################################################
# Strings.                                                                                                             #
########################################################################################################################

proc read_string_with_length(stream: FileStream): string =
  let size = stream.read(uint16)
  stream.read_str(size)

proc write_string_with_length(stream: FileStream, string: string) =
  if string.len > cast[int](high(uint16)):
    fail(fmt"Cannot write strings with more than {high(uint16)} bytes.")

  stream.write(cast[uint16](string.len))
  stream.write_str(string)

########################################################################################################################
# Type names.                                                                                                          #
########################################################################################################################

method collect_type_name_dependencies*(tpe: PoemType, dependencies: var seq[string]) {.base, locks: "unknown".} =
  ## Adds all type names which this type references to `dependencies`. May add duplicates.
  discard

method collect_type_name_dependencies*(tpe: PoemXaryType, dependencies: var seq[string]) {.locks: "unknown".} =
  for t in tpe.types:
    collect_type_name_dependencies(t, dependencies)

method collect_type_name_dependencies*(tpe: PoemShapeType, dependencies: var seq[string]) {.locks: "unknown".} =
  for t in tpe.property_types:
    collect_type_name_dependencies(t, dependencies)

method collect_type_name_dependencies*(tpe: PoemNamedType, dependencies: var seq[string]) {.locks: "unknown".} =
  dependencies.add(tpe.name)
  for t in tpe.type_arguments:
    collect_type_name_dependencies(t, dependencies)

proc collect_type_name_dependencies*(type_parameter: PoemTypeParameter, dependencies: var seq[string]) =
  ## Adds all type names which the bounds of `type_parameter` reference to `dependencies`. May add duplicates.
  collect_type_name_dependencies(type_parameter.lower_bound, dependencies)
  collect_type_name_dependencies(type_parameter.upper_bound, dependencies)
