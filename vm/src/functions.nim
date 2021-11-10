from instructions import Instruction
from types import Type, TupleType
from values import TaggedValue

type
  MultiFunction* = ref object
    name*: string
    functions*: seq[Function]

    ## The `hierarchy` is initialized once all functions associated with the multi-function have been loaded.
    # TODO (vm): We can build the hierarchy later.
    # TODO (vm): We should keep a list (or sub-hierarchy) of functions with one argument, two arguments, etc., for each
    #            DispatchX operation so that we can quickly access the relevant subset of functions.
    #hierarchy: MultiFunctionHierarchy

  # TODO (vm): Add type parameters.
  Function* = ref object
    multi_function*: MultiFunction
    input_type*: TupleType
    output_type*: Type

    ## An abstract function has no instructions and cannot be invoked.
    is_abstract*: bool

    register_count*: uint16
    instructions*: seq[Instruction]

    ## `constants` will be initialized after all type, value, and multi-function constants have been resolved.
    constants*: Constants

    ## These fields contain precomputed sizes and offsets for faster frame creation. They will be calculated by
    ## `evaluator/init_function`.
    frame_size*: uint16
    frame_registers_offset*: uint16

  ## A Constants object provides quick access to predefined types, values, and multi-functions. It may be shared across
  ## multiple function definitions. All entries are separately accessed by a uint16.
  Constants* = ref object
    types*: seq[Type]
    values*: seq[TaggedValue]
    multi_functions*: seq[MultiFunction]

proc new_constants*(): Constants = Constants(types: @[], values: @[], multi_functions: @[])
