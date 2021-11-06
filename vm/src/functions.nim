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

proc get_dispatch_target*(mf: MultiFunction, value: TaggedValue): Function =
  # TODO (vm): This optimization is not quite correct. We can assume that the compiler produces valid calls in the
  #            bytecode, but we have to take lower bounds into account.
  # TODO (vm): This is an optimization I would like to be carried out in a preprocessing phase which could for example
  #            replace `dispatch` with `call` instructions.
  if mf.functions.len == 1:
    return mf.functions[0]
  else:
    quit("Error: Multi-function dispatch not yet implemented.")
