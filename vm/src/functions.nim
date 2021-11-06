import std/strformat

from instructions import Instruction
from types import Type, TupleType
from values import TaggedValue

type
  MultiFunction* = ref object
    name*: string
    functions*: seq[Function]

    ## The `hierarchy` is initialized once all functions associated with the multi-function have been loaded.
    # TODO (vm): We can build the hierarchy later. For now, dispatch can consist of subtyping against input types in a
    #            flat list of functions. This is obviously wrong, but will suffice for very simple examples.
    # TODO (vm): We should keep a list (or sub-hierarchy) of functions with one argument, two arguments, etc., for each
    #            DispatchX operation so that we can quickly access the relevant subset of functions.
    #hierarchy: MultiFunctionHierarchy

  # TODO (vm): Allow functions to be abstract? This is probably relevant for the hierarchy.
  # TODO (vm): Add type parameters.
  Function* = ref object
    multi_function*: MultiFunction
    input_type*: TupleType
    output_type*: Type

    register_count*: uint16
    code*: seq[Instruction]  # TODO (vm): Rename to `instructions`.

    ## `constants` will be initialized after all type, value, and multi-function constants have been resolved.
    constants*: Constants

    ## These fields contain precomputed sizes and offsets for faster frame creation. They will be calculated by
    ## `evaluator/init_function`.
    frame_size*: uint16
    frame_registers_offset*: uint16

  ## A Constants object provides quick access to predefined types, multi-functions, and values. It may be shared
  ## between multiple function definitions.
  Constants* = ref object
    types*: seq[Type]
    multi_functions*: seq[MultiFunction]
    values*: seq[TaggedValue]

proc new_constants*(): Constants = Constants(types: @[], multi_functions: @[], values: @[])

########################################################################################################################
# Dispatch.                                                                                                            #
########################################################################################################################

# TODO (vm): Maybe move this to its own file, `dispatch.nim`, especially when the code becomes very complex once we
#            implement some optimizations.

proc get_dispatch_target*(mf: MultiFunction, input_type: TupleType): Function =
  # TODO (vm): This is probably the slowest non-esoteric implementation of dispatch. However, it will suffice for now
  #            until we can spend more time making it fast.
  var candidates = new_seq_of_cap[Function](8)
  for function in mf.functions:
    # TODO (vm): This should be the fit, of course.
    if types.is_subtype(input_type, function.input_type):
      candidates.add(function)

  # Well, for this to be correct, we also have to filter out all the functions that aren't most specific, because we
  # haven't built a function hierarchy yet. This is very slow, but works.
  var most_specific = new_seq_of_cap[Function](8)
  for candidate in candidates:
    var is_most_specific = true
    for f2 in candidates:
      if cast[pointer](candidate) != cast[pointer](f2):
        if types.is_subtype(f2.input_type, candidate.input_type):
          is_most_specific = false
          break
    if is_most_specific: most_specific.add(candidate)

  # TODO (vm): Improve these error messages by including the input type and the candidates (if ambiguous).
  if most_specific.len == 0:
    quit(fmt"Cannot call multi-function {mf.name}: empty fit.")
  elif most_specific.len > 1:
    quit(fmt"Cannot call multi-function {mf.name}: ambiguous call.")
  most_specific[0]

proc get_dispatch_target*(mf: MultiFunction, value: TaggedValue): Function =
  # TODO (vm): This optimization is not quite correct. We can assume that the compiler produces valid calls in the
  #            bytecode, but we have to take lower bounds into account.
  # TODO (vm): This is an optimization I would like to be carried out in a preprocessing phase which could for example
  #            replace `dispatch` with `call` instructions.
  if mf.functions.len == 1:
    return mf.functions[0]
  else:
    # TODO (vm): We can technically omit creating the tuple if we're a little smart about it. But this is an
    #            optimization for later.
    get_dispatch_target(mf, types.tpl([values.type_of(value)]))
