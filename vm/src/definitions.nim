from instructions import Instruction
from types import Type, TupleType
from values import TaggedValue

type
  ## A global variable is a uniquely named variable that is accessible from any function. A global variable may either
  ## be eager or lazy. The former immediately receives a value, the latter is initialized only when its value is first
  ## requested, using an initializing function.
  GlobalVariable* = ref object
    name*: string
    value*: TaggedValue

    ## Whether the global variable's `value` has already been initialized.
    is_initialized*: bool

    ## The function that is supposed to initialize the global variable. It is nil for eager global variables.
    initializer*: Function

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
    global_variables*: seq[GlobalVariable]
    multi_functions*: seq[MultiFunction]

## Creates an already initialized global variable from the given name and value.
proc new_global_eager*(name: string, value: TaggedValue): GlobalVariable =
  GlobalVariable(name: name, value: value, is_initialized: true, initializer: nil)

## Creates a lazy global variable from the given name and initializer.
proc new_global_lazy*(name: string, initializer: Function): GlobalVariable =
  GlobalVariable(name: name, value: values.tag_reference(nil), is_initialized: false, initializer: initializer)

proc new_constants*(): Constants = Constants(types: @[], values: @[], multi_functions: @[])
