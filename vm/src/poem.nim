import binstreams
import std/os
import std/strformat

from functions import Function, Constants
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
  Poem = ref object
    constants*: PoemConstants
    functions*: seq[PoemFunction]

  ## An unresolved constants table.
  PoemConstants = ref object
    #types*: seq[Type]
    multi_functions*: seq[string]
    #values*: seq[TaggedValue]

  ## An unresolved function.
  PoemFunction = ref object
    name: string
    input_type: PoemType
    output_type: PoemType
    register_count: uint16
    instructions: seq[Instruction]

  ## An unresolved type.
  PoemType = ref object of RootObj
    discard  # TODO (vm): Implement.

  PoemBasicType = ref object of PoemType
    tpe: Type

  ## This unresolved type contains any number of child types and represents sums, functions, lists, etc.
  PoemXaryType = ref object of PoemType
    kind: Kind
    types: seq[PoemType]

  PoemSymbolType = ref object of PoemType
    name: string

  PoemNamedType = ref object of PoemType
    name: string
    arguments: seq[PoemType]

########################################################################################################################
# Reading bytecode.                                                                                                    #
########################################################################################################################

proc read_constants(stream: FileStream): PoemConstants
proc read_function(stream: FileStream): PoemFunction
proc read_type(stream: FileStream): PoemType
proc read_instruction(stream: FileStream): Instruction
proc read_string(stream: FileStream): string

template read_many_with_count(stream: FileStream, result_type: untyped, count: uint, read_one): untyped =
  var results = new_seq_of_cap[result_type](count)
  var i: uint = 0
  while i < count:
    results.add(read_one(stream))
    i += 1
  results

template read_many(stream: FileStream, result_type: untyped, count_type, read_one): untyped =
  let count = stream.read(count_type)
  read_many_with_count(stream, result_type, count, read_one)

proc read*(path: string): Poem =
  if unlikely(not file_exists(path)):
    raise new_exception(IOError, fmt"""Poem file "{path}" does not exist.""")

  let stream = new_file_stream(path, big_endian, fmRead)
  defer: stream.close()

  # Check that the file starts with "poem".
  let magic_string = stream.read_str(4)
  if magic_string != "poem":
    raise new_exception(IOError, fmt"""Poem file "{path}" has an illegal file header. The file must begin with the ASCII string `poem`.""")

  let constants = read_constants(stream)
  let functions = read_many(stream, PoemFunction, uint16, read_function)
  Poem(
    constants: constants,
    functions: functions,
  )

# TODO (vm): Read type and value constants.
proc read_constants(stream: FileStream): PoemConstants =
  let multi_functions = read_many(stream, string, uint16, read_string)

  PoemConstants(
    multi_functions: multi_functions,
  )

proc read_function(stream: FileStream): PoemFunction =
  let name = read_string(stream)
  let input_type = read_type(stream)
  let output_type = read_type(stream)
  let register_count = stream.read(uint16)
  let instructions = read_many(stream, Instruction, uint16, read_instruction)

  PoemFunction(
    name: name,
    input_type: input_type,
    output_type: output_type,
    register_count: register_count,
    instructions: instructions,
  )

const
  tkBasic = 0
  tkFixedSize = 1
  tkSum = 2
  tkIntersection = 3
  tkTuple = 4
  tkShape = 5
  tkNamed = 6

  btAny = 0
  btNothing = 1
  btInt = 2
  btReal = 3
  btBoolean = 4
  btString = 5

  fsFunction = 0
  fsList = 1
  fsMap = 2
  fsSymbol = 3

template read_xary_type(stream: FileStream, xary_kind: Kind, metadata: uint8): untyped =
  let types = read_many_with_count(stream, PoemType, metadata, read_type)
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
      let input = read_type(stream)
      let output = read_type(stream)
      PoemXaryType(kind: Kind.Function, types: @[input, output])
    of fsList:
      let element = read_type(stream)
      PoemXaryType(kind: Kind.List, types: @[element])
    of fsMap:
      let key = read_type(stream)
      let value = read_type(stream)
      PoemXaryType(kind: Kind.Map, types: @[key, value])
    of fsSymbol:
      let name = read_string(stream)
      PoemSymbolType(name: name)
    else: raise new_exception(IOError, fmt"Unknown fixed-size type {metadata}.")

  of tkSum: read_xary_type(stream, Kind.Sum, metadata)
  of tkIntersection: read_xary_type(stream, Kind.Intersection, metadata)
  of tkTuple: read_xary_type(stream, Kind.Tuple, metadata)

  of tkShape:
    # TODO (vm): Implement shape type reading.
    raise new_exception(IOError, "Reading shape types is not yet implemented.")

  of tkNamed:
    let name = read_string(stream)
    let arguments = read_many_with_count(stream, PoemType, metadata, read_type)
    PoemNamedType(name: name, arguments: arguments)

  else: raise new_exception(IOError, fmt"Unknown type kind {kind}.")

proc read_instruction(stream: FileStream): Instruction =
  new_instruction(
    cast[Operation](stream.read(uint16)),
    stream.read(uint16),
    stream.read(uint16),
    stream.read(uint16),
  )

proc read_string(stream: FileStream): string =
  let size = stream.read(uint16)
  stream.read_str(size)

########################################################################################################################
# Writing bytecode.                                                                                                    #
########################################################################################################################

proc write*() =
  discard
