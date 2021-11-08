import std/sequtils
import std/strformat
import std/tables
import sugar

from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from poems import Poem, PoemConstants, PoemFunction, PoemType, PoemBasicType, PoemXaryType, PoemSymbolType, PoemNamedType
from types import Kind, Type, TupleType

type
  ## The Universe object represents all combined information available about the current Lore program.
  Universe* = ref object
    multi_functions*: Table[string, MultiFunction]

proc ensure_multi_function(universe: Universe, name: string)

proc resolve(universe: Universe, poem: Poem)
proc resolve(universe: Universe, poem_constants: PoemConstants): Constants
proc resolve(universe: Universe, poem_function: PoemFunction, constants: Constants)
proc resolve(universe: Universe, poem_type: PoemType): Type

## Resolves a Universe from the given set of Poem definitions.
proc resolve*(poems: seq[Poem]): Universe =
  var universe = Universe(multi_functions: init_table[string, MultiFunction]())

  # We have to create empty multi-functions right away so that the constants tables for each Poem can contain the right
  # multi-function references.
  for poem in poems:
    for poem_function in poem.functions:
      universe.ensure_multi_function(poem_function.name)

  for poem in poems:
    universe.resolve(poem)
  universe

## Ensures that the universe contains a multi-function with the given full name.
proc ensure_multi_function(universe: Universe, name: string) =
  if not (name in universe.multi_functions):
    universe.multi_functions[name] = MultiFunction(
      name: name,
      functions: @[],
    )

proc resolve(universe: Universe, poem: Poem) =
  let constants = universe.resolve(poem.constants)
  for poem_function in poem.functions:
    universe.resolve(poem_function, constants)

proc resolve(universe: Universe, poem_constants: PoemConstants): Constants =
  let constants = new_constants()

  for poem_type in poem_constants.types:
    constants.types.add(universe.resolve(poem_type))

  constants.values = poem_constants.values

  # At this point, all multi-functions will be known by reference, so we can immediately build the constants table.
  for name in poem_constants.multi_functions:
    constants.multi_functions.add(universe.multi_functions[name])

  constants

proc resolve(universe: Universe, poem_function: PoemFunction, constants: Constants) =
  let multi_function = universe.multi_functions[poem_function.name]

  let input_type_raw = universe.resolve(poem_function.input_type)
  if input_type_raw.kind != Kind.Tuple:
    quit(fmt"Functions must always have a tuple as their input type. Function name: {poem_function.name}.")
  let input_type = cast[TupleType](input_type_raw)

  let output_type = universe.resolve(poem_function.output_type)
  let function = Function(
    multi_function: multi_function,
    input_type: input_type,
    output_type: output_type,
    register_count: poem_function.register_count,
    instructions: poem_function.instructions,
    constants: constants,
  )
  init_frame_stats(function)

  multi_function.functions.add(function)

method resolve(poem_type: PoemType, universe: Universe): Type {.base, locks: "unknown".} =
  quit("Please implement `resolve` for all PoemTypes.")

method resolve(poem_type: PoemBasicType, universe: Universe): Type {.locks: "unknown".} = poem_type.tpe

method resolve(poem_type: PoemXaryType, universe: Universe): Type =
  if poem_type.kind != Kind.Tuple:
    quit("Xary types except for tuples cannot be resolved yet.")

  if poem_type.types.len == 0:
    types.unit
  else:
    let elements = poem_type.types.map(t => t.resolve(universe))
    types.tpl(elements)

method resolve(poem_type: PoemSymbolType, universe: Universe): Type {.locks: "unknown".} = types.symbol(poem_type.name)

method resolve(poem_type: PoemNamedType, universe: Universe): Type {.locks: "unknown".} =
  quit(fmt"Cannot resolve named types yet. Name: {poem_type.name}.")

proc resolve(universe: Universe, poem_type: PoemType): Type =
  poem_type.resolve(universe)
