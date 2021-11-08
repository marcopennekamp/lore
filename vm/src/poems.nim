import binstreams
import std/os
import std/strformat

from instructions import Operation, Instruction, new_instruction
from types import Kind, Type

type
  ## A Poem is essentially a single unit of bytecode which contains type and multi-function definitions. A program may
  ## consist of many Poems. An explicit structure is not prescribed. A Lore compiler may decide to put a whole program
  ## into a single Poem (as long as the Constants table suffices), put each function and type definition into its own
  ## Poem, or anything in between.
  ##
  ## After a Poem has been read, it contains mostly unresolved objects, such as unresolved types and functions.
  ## Resolution entails creating objects in the right order (e.g. creating types first) and then building the required
  ## structures with the correct pointers.
  ##
  ## `.poem` files must be encoded in big endian.
  Poem* = ref object
    constants*: PoemConstants
    functions*: seq[PoemFunction]

  ## An unresolved constants table.
  PoemConstants* = ref object
    types*: seq[PoemType]
    values*: seq[PoemValue]
    multi_functions*: seq[string]

  ## An unresolved function.
  PoemFunction* = ref object
    name*: string
    input_type*: PoemType
    output_type*: PoemType
    register_count*: uint16
    instructions*: seq[Instruction]

  ## An unresolved type.
  PoemType* = ref object of RootObj
    discard

  PoemBasicType* = ref object of PoemType
    tpe*: Type

  ## This unresolved type contains any number of child types and represents sums, functions, lists, etc.
  PoemXaryType* = ref object of PoemType
    kind*: Kind
    types*: seq[PoemType]

  PoemSymbolType* = ref object of PoemType
    name*: string

  PoemNamedType* = ref object of PoemType
    name*: string
    arguments*: seq[PoemType]

  ## An unresolved value.
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

let int_type*: PoemType = PoemBasicType(tpe: types.int)
let real_type*: PoemType = PoemBasicType(tpe: types.real)
let boolean_type*: PoemType = PoemBasicType(tpe: types.boolean)
let string_type*: PoemType = PoemBasicType(tpe: types.string)
let unit_type*: PoemType = PoemXaryType(kind: Kind.Tuple, types: @[])

proc tuple_type*(poem_types: open_array[PoemType]): PoemType = PoemXaryType(kind: Kind.Tuple, types: @poem_types)

proc string_value*(value: string): PoemValue = PoemStringValue(string: value)

proc fail(message: string) {.noreturn.} = raise new_exception(IOError, message)

const
  tkBasic = 0'u8
  tkFixedSize = 1'u8
  tkSum = 2'u8
  tkIntersection = 3'u8
  tkTuple = 4'u8
  tkShape = 5'u8
  tkNamed = 6'u8

  btAny = 0'u8
  btNothing = 1'u8
  btInt = 2'u8
  btReal = 3'u8
  btBoolean = 4'u8
  btString = 5'u8

  fsFunction = 0'u8
  fsList = 1'u8
  fsMap = 2'u8
  fsSymbol = 3'u8

# TODO (vm): Organize this file by domain (functions, types, values) instead of reading/writing.

########################################################################################################################
# Reading bytecode.                                                                                                    #
########################################################################################################################

proc read_constants(stream: FileStream): PoemConstants
proc read_type(stream: FileStream): PoemType
proc read_value(stream: FileStream): PoemValue
proc read_function(stream: FileStream): PoemFunction
proc read_instruction(stream: FileStream): Instruction
proc read_string_with_length(stream: FileStream): string

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
  let functions = stream.read_many_with_count(PoemFunction, uint16, read_function)
  Poem(
    constants: constants,
    functions: functions,
  )

proc read_constants(stream: FileStream): PoemConstants =
  let types = stream.read_many_with_count(PoemType, uint16, read_type)
  let values = stream.read_many_with_count(PoemValue, uint16, read_value)
  let multi_functions = stream.read_many_with_count(string, uint16, read_string_with_length)

  PoemConstants(
    types: types,
    values: values,
    multi_functions: multi_functions,
  )

template read_xary_type(stream: FileStream, xary_kind: Kind, metadata: uint8): untyped =
  let types = stream.read_many(PoemType, metadata, read_type)
  PoemXaryType(kind: xary_kind, types: types)

proc read_type(stream: FileStream): PoemType =
  let tag = stream.read(uint8)
  let kind = tag shr 5
  let metadata = tag and 0b11111

  case kind
  of tkBasic:
    let tpe = case metadata
    of btAny: types.any
    of btNothing: types.nothing
    of btInt: types.int
    of btReal: types.real
    of btBoolean: types.boolean
    of btString: types.string
    else: raise new_exception(IOError, fmt"Unknown basic type {metadata}.")
    PoemBasicType(tpe: tpe)

  of tkFixedSize:
    case metadata
    of fsFunction:
      let input = stream.read_type()
      let output = stream.read_type()
      PoemXaryType(kind: Kind.Function, types: @[input, output])
    of fsList:
      let element = stream.read_type()
      PoemXaryType(kind: Kind.List, types: @[element])
    of fsMap:
      let key = stream.read_type()
      let value = stream.read_type()
      PoemXaryType(kind: Kind.Map, types: @[key, value])
    of fsSymbol:
      let name = stream.read_string_with_length()
      PoemSymbolType(name: name)
    else: raise new_exception(IOError, fmt"Unknown fixed-size type {metadata}.")

  of tkSum: stream.read_xary_type(Kind.Sum, metadata)
  of tkIntersection: stream.read_xary_type(Kind.Intersection, metadata)
  of tkTuple: stream.read_xary_type(Kind.Tuple, metadata)

  of tkShape:
    # TODO (vm): Implement shape type reading.
    raise new_exception(IOError, "Reading shape types is not yet implemented.")

  of tkNamed:
    let name = stream.read_string_with_length()
    let arguments = stream.read_many(PoemType, metadata, read_type)
    PoemNamedType(name: name, arguments: arguments)

  else: raise new_exception(IOError, fmt"Unknown type kind {kind}.")

proc read_value(stream: FileStream): PoemValue =
  let tpe = stream.read_type()
  if tpe of PoemBasicType:
    case cast[PoemBasicType](tpe).tpe.kind
    of Kind.Int:
      let value = stream.read(int64)
      PoemIntValue(int: value)
    of Kind.Real:
      let value = stream.read(float64)
      PoemRealValue(real: value)
    of Kind.Boolean:
      let value = stream.read(uint8)
      PoemBooleanValue(boolean: value == 1)
    of Kind.String:
      let value = stream.read_string_with_length()
      PoemStringValue(string: value)
    else: fail("Invalid kind for basic type value.")
  elif tpe of PoemXaryType:
    let xary = cast[PoemXaryType](tpe)
    case xary.kind
    of Kind.Tuple:
      let elements = stream.read_many(PoemValue, cast[uint](xary.types.len), read_value)
      PoemTupleValue(tpe: tpe, elements: elements)
    else:
      fail("Only tuple values are supported for now.")
  else:
    fail("Symbol and named values aren't supported for now.")

proc read_function(stream: FileStream): PoemFunction =
  let name = stream.read_string_with_length()
  let input_type = stream.read_type()
  let output_type = stream.read_type()
  let register_count = stream.read(uint16)
  let instructions = stream.read_many_with_count(Instruction, uint16, read_instruction)

  PoemFunction(
    name: name,
    input_type: input_type,
    output_type: output_type,
    register_count: register_count,
    instructions: instructions,
  )

proc read_instruction(stream: FileStream): Instruction =
  new_instruction(
    cast[Operation](stream.read(uint16)),
    stream.read(uint16),
    stream.read(uint16),
    stream.read(uint16),
    stream.read(uint16),
  )

proc read_string_with_length(stream: FileStream): string =
  let size = stream.read(uint16)
  stream.read_str(size)

########################################################################################################################
# Writing bytecode.                                                                                                    #
########################################################################################################################

proc write_constants(stream: FileStream, constants: PoemConstants)
proc write_type(stream: FileStream, tpe: PoemType)
proc write_value(stream: FileStream, value: PoemValue)
proc write_function(stream: FileStream, function: PoemFunction)
proc write_instruction(stream: FileStream, instruction: Instruction)
proc write_string_with_length(stream: FileStream, string: string)

method write(tpe: PoemType, stream: FileStream) {.base, locks: "unknown".}
method write(value: PoemValue, stream: FileStream) {.base, locks: "unknown".}

template write_many(stream: FileStream, items, write_one): untyped =
  for item in items:
    write_one(stream, item)

template write_many_with_count(stream: FileStream, items, count_type, write_one): untyped =
  let count = count_type(items.len)
  stream.write(count)
  stream.write_many(items, write_one)

proc write*(path: string, poem: Poem) =
  let stream = new_file_stream(path, big_endian, fmWrite)
  defer: stream.close()

  stream.write_str("poem")
  stream.write_constants(poem.constants)
  stream.write_many_with_count(poem.functions, uint16, write_function)

proc write_constants(stream: FileStream, constants: PoemConstants) =
  stream.write_many_with_count(constants.types, uint16, write_type)
  stream.write_many_with_count(constants.values, uint16, write_value)
  stream.write_many_with_count(constants.multi_functions, uint16, write_string_with_length)

proc write_type(stream: FileStream, tpe: PoemType) =
  tpe.write(stream)

proc write_value(stream: FileStream, value: PoemValue) =
  value.write(stream)

proc write_function(stream: FileStream, function: PoemFunction) =
  stream.write_string_with_length(function.name)
  stream.write_type(function.input_type)
  stream.write_type(function.output_type)
  stream.write(function.register_count)
  stream.write_many_with_count(function.instructions, uint16, write_instruction)

proc write_instruction(stream: FileStream, instruction: Instruction) =
  stream.write(cast[uint16](instruction.operation))
  for argument in instruction.arguments:
    stream.write(argument.uint_value)

proc write_string_with_length(stream: FileStream, string: string) =
  if string.len > cast[int](high(uint16)):
    fail(fmt"Cannot write strings with more than {high(uint16)}")

  stream.write(cast[uint16](string.len))
  stream.write_str(string)

proc write_type_tag(stream: FileStream, tag_kind: uint8, tag_metadata: uint8) =
  let tag: uint8 = (tag_kind shl 5) or tag_metadata
  stream.write(tag)

proc write_type_tag(stream: FileStream, kind: Kind, child_count: uint8) =
  let (tag_kind, tag_metadata) = case kind
  of Kind.TypeVariable: (tkNamed, 0'u8)
  of Kind.Any: (tkBasic, btAny)
  of Kind.Nothing: (tkBasic, btNothing)
  of Kind.Int: (tkBasic, btInt)
  of Kind.Real: (tkBasic, btReal)
  of Kind.Boolean: (tkBasic, btBoolean)
  of Kind.String: (tkBasic, btString)
  of Kind.Sum: (tkSum, child_count)
  of Kind.Intersection: (tkIntersection, child_count)
  of Kind.Tuple: (tkTuple, child_count)
  of Kind.Function: (tkFixedSize, fsFunction)
  of Kind.List: (tkFixedSize, fsList)
  of Kind.Map: (tkFixedSize, fsMap)
  of Kind.Symbol: (tkFixedSize, fsSymbol)
  of Kind.Shape: (tkShape, child_count)
  of Kind.Trait: (tkNamed, child_count)
  of Kind.Struct: (tkNamed, child_count)
  stream.write_type_tag(tag_kind, tag_metadata)

proc write_type_tag(stream: FileStream, kind: Kind) =
  stream.write_type_tag(kind, 0)

method write(tpe: PoemType, stream: FileStream) {.base, locks: "unknown".} =
  quit("Please implement `write` for all PoemTypes.")

# TODO (vm): Implement shape type writing.
method write(tpe: PoemBasicType, stream: FileStream) {.locks: "unknown".} =
  stream.write_type_tag(tpe.tpe.kind)

proc length_to_tag_metadata(length: int): uint8 =
  if unlikely(length > 31):
    fail(fmt"Types cannot have more than 31 type children.")
  return cast[uint8](length)

method write(tpe: PoemXaryType, stream: FileStream) =
  let child_count = uint64(tpe.types.len)
  if child_count > 31:
    fail(fmt"Types of kind {tpe.kind} cannot have more than 31 type children.")

  stream.write_type_tag(tpe.kind, length_to_tag_metadata(tpe.types.len))
  stream.write_many(tpe.types, write_type)

method write(tpe: PoemSymbolType, stream: FileStream) {.locks: "unknown".} =
  stream.write_type_tag(Kind.Symbol)
  stream.write_string_with_length(tpe.name)

method write(tpe: PoemNamedType, stream: FileStream) =
  stream.write_type_tag(tkNamed, length_to_tag_metadata(tpe.arguments.len))
  stream.write_string_with_length(tpe.name)
  stream.write_many(tpe.arguments, write_type)

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
  stream.write(if value.boolean: 1'u8 else: 0'u8)

method write(value: PoemStringValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(string_type)
  stream.write_string_with_length(value.string)

method write(value: PoemTupleValue, stream: FileStream) {.locks: "unknown".} =
  stream.write_type(value.tpe)
  stream.write_many(value.elements, write_value)
