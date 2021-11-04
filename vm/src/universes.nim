import std/sequtils
import std/strformat
import std/tables
from std/tables import Table, init_table
import sugar

from evaluator import init_frame_stats
from functions import MultiFunction, Function, Constants, new_constants
from poems import Poem, PoemFunction, PoemType, PoemBasicType, PoemXaryType, PoemSymbolType, PoemNamedType
from types import Kind, Type, TupleType

type
  ## The Universe object represents all combined information available about the current Lore program.
  Universe* = ref object
    multi_functions*: Table[string, MultiFunction]

proc ensure_multi_function(universe: Universe, name: string)

proc resolve(universe: Universe, poem: Poem)
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
  # At this point, all multi-functions will have been assigned a reference, so we can immediately build the constants
  # table.
  let constants = new_constants()
  for name in poem.constants.multi_functions:
    constants.multi_functions.add(universe.multi_functions[name])

  for poem_function in poem.functions:
    universe.resolve(poem_function, constants)

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
    code: poem_function.instructions,
    constants: constants,
  )
  init_frame_stats(function)

  multi_function.functions.add(function)

method resolve(poem_type: PoemType, universe: Universe): Type

proc resolve(universe: Universe, poem_type: PoemType): Type =
  poem_type.resolve(universe)

method resolve(poem_type: PoemType, universe: Universe): Type =
  quit("Please implement `resolve` for all PoemTypes.")

method resolve(poem_type: PoemBasicType, universe: Universe): Type = poem_type.tpe

method resolve(poem_type: PoemXaryType, universe: Universe): Type =
  if poem_type.kind != Kind.Tuple:
    quit("Xary types except for tuples cannot be resolved yet.")

  if poem_type.types.len == 0:
    types.unit
  else:
    let elements = poem_type.types.map(t => t.resolve(universe))
    types.tpl(elements)

method resolve(poem_type: PoemSymbolType, universe: Universe): Type = types.symbol(poem_type.name)

method resolve(poem_type: PoemNamedType, universe: Universe): Type =
  quit(fmt"Cannot resolve named types yet. Name: {poem_type.name}.")
