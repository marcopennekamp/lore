import std/strformat

import imseqs
from instructions import Instruction
from types import TypeParameter, Type, TupleType, FunctionType, MetaShape, Schema, new_function_type, bounds_contain,
                  substitute
from values import TaggedValue, LambdaContext, tag_reference

type
  Frame* = object
    ## A frame represents the memory that the evaluation of a single monomorphic or polymorphic function call requires.
    ## The memory for all frames must be preallocated before the evaluator is invoked.
    ##
    ## Frames are part of `definitions` because some intrinsics require access to frames.
    function*: Function
    type_arguments*: ImSeq[Type]
    lambda_context*: LambdaContext
    registers*: UncheckedArray[uint64]
      ## Registers may contain TaggedValues and Types. Whether a register currently contains a value or a type is
      ## solely defined by the operations that act on the registers.
  FramePtr* = ptr Frame

  Intrinsic* = ref object
    ## An intrinsic is a function built into the virtual machine that can be called from bytecode. Every intrinsic
    ## expects the current frame and a list of arguments.
    ##
    ## To allow intrinsics to call Lore function values (e.g. a lambda for `lore.list.map`), intrinsics are
    ## *frame-aware* as they receive the frame as their first argument.
    name*: string
    function*: IntrinsicFunction
    arity*: int

  IntrinsicFunction* = proc (frame: FramePtr, arguments: open_array[TaggedValue]): TaggedValue {.nimcall.}

  GlobalVariable* = ref object
    ## A global variable is a uniquely named variable that is accessible from any function. A global variable may
    ## either be eager or lazy. Eager variables are immediately initialized with a value. Lazy variables are
    ## initialized only when its value is first requested.
    name*: string
    value*: TaggedValue

    is_initialized*: bool
      ## Whether the global variable's `value` has already been initialized.

    initializer*: FunctionInstance
      ## The function instance that is supposed to initialize the global variable. It's empty for eager variables.

  MultiFunction* = ref object
    name*: string
    functions*: seq[Function]

    # TODO (vm): We can build the hierarchy later.
    # TODO (vm): We should keep a list (or sub-hierarchy) of functions with one argument, two arguments, etc., for each
    #            DispatchX operation so that we can quickly access the relevant subset of functions.
    #hierarchy: MultiFunctionHierarchy
      ## The `hierarchy` is initialized once all functions associated with the multi-function have been loaded.

  Function* = ref object
    multi_function*: MultiFunction
    type_parameters*: ImSeq[TypeParameter]
    input_type*: TupleType
    output_type*: Type

    is_abstract*: bool
      ## An abstract function has no instructions and cannot be invoked.

    monomorphic_instance*: FunctionInstance
      ## The monomorphic instance of a function is only defined if the function is monomorphic. This instance can be
      ## used to bypass creating new function instances every time dispatch is resolved, even though the type argument
      ## list will always be empty.

    register_count*: uint16
    instructions*: seq[Instruction]

    constants*: Constants
      ## `constants` will be initialized after all type, value, and multi-function constants have been resolved.

    frame_size*: uint16
      ## `frame_size` is a precomputed size for faster frame creation. It's initialized by `init_frame_stats`.

  FunctionInstance* = object
    ## A function instance is a function with assigned type arguments. To avoid allocations on the heap, function
    ## instances may sometimes be placed on the stack or passed by value.
    function*: Function
    type_arguments*: ImSeq[Type]

  # TODO (vm): To perhaps optimize constants access by removing one layer of indirection, we could make the uint16
  #            index absolute and then turn the Constants table into a contiguous unchecked array of 8-byte values.
  #            For example, if we have a constants table with 1 type, 2 values, and 1 global variable, 0 would access
  #            the type, 1 and 2 the values, and 3 the global variable. The evaluator would have to cast the resulting
  #            constant accordingly, but this is basically a no-op once optimized.

  Constants* = ref object
    ## A Constants object provides quick access to predefined types, values, names, intrinsics, global variables,
    ## multi-functions, and meta shapes. It may be shared across multiple function definitions. All entries are
    ## separately accessed by a uint16 index.
    ##
    ## Types are implicitly separated into monomorphic and polymorphic entities. A monomorphic type is guaranteed to
    ## contain no type variables and can be used as is. A polymorphic type contains at least one type variable. Such
    ## types must be used with instructions containing the word `Poly`. All type variables are substituted using the
    ## current function instance's type arguments.
    types*: seq[Type]
    values*: seq[TaggedValue]
    names*: seq[string]
      ## Constant names are used for accessing properties.
    intrinsics*: seq[Intrinsic]
    schemas*: seq[Schema]
    global_variables*: seq[GlobalVariable]
    multi_functions*: seq[MultiFunction]
    function_instances*: seq[ptr FunctionInstance]
    meta_shapes*: seq[MetaShape]
      ## A constant table's meta shapes are used to allocate new shape instances via the requisite instructions. They
      ## are not referenced by constant table types or values, nor by any other type declarations.

const operand_list_limit*: int = 256

proc `$`*(function: Function): string

########################################################################################################################
# Global variables.                                                                                                    #
########################################################################################################################

proc new_eager_global*(name: string, value: TaggedValue): GlobalVariable =
  GlobalVariable(name: name, value: value, is_initialized: true)

proc new_lazy_global*(name: string, initializer: FunctionInstance): GlobalVariable =
  GlobalVariable(name: name, value: tag_reference(nil), is_initialized: false, initializer: initializer)

########################################################################################################################
# Functions and instances.                                                                                             #
########################################################################################################################

proc is_single_function*(mf: MultiFunction): bool {.inline.} = mf.functions.len == 1

proc name*(function: Function): string {.inline.} = function.multi_function.name
proc arity*(function: Function): int {.inline.} = function.input_type.elements.len

proc is_monomorphic*(function: Function): bool {.inline.} = function.type_parameters.len == 0
proc is_polymorphic*(function: Function): bool {.inline.} = function.type_parameters.len > 0

proc new_function_instance*(function: Function, type_arguments: ImSeq[Type]): ptr FunctionInstance =
  let instance = cast[ptr FunctionInstance](alloc0(sizeof(FunctionInstance)))
  instance.function = function
  instance.type_arguments = type_arguments
  instance

proc instantiate*(function: Function, type_arguments: ImSeq[Type]): ptr FunctionInstance =
  # TODO (vm): When using this function, we technically only need to check lower bounds of covariant parameters and
  #            upper bounds of contravariant parameters, as we can reasonably assume that the compiler has covered the
  #            other direction. This could be an optimization specifically for instantiations from instructions such
  #            as `CallPoly`.
  if function.is_monomorphic: addr function.monomorphic_instance
  else:
    let type_parameters = function.type_parameters
    for i in 0 ..< type_parameters.len:
      if not bounds_contain(type_parameters[i], type_arguments[i], type_arguments.to_open_array):
        quit(fmt"Cannot instantiate function `{function}` with type arguments `{type_arguments}`.")
    new_function_instance(function, type_arguments)

proc get_function_type*(instance: ptr FunctionInstance): FunctionType =
  ## Constructs a new function type from the given function instance.
  let input_type = instance.function.input_type.substitute(instance.type_arguments.to_open_array)
  let output_type = instance.function.output_type.substitute(instance.type_arguments.to_open_array)
  new_function_type(cast[TupleType](input_type), output_type)

proc init_frame_stats*(function: Function) =
  const preamble_size = sizeof(Frame)
  function.frame_size = cast[uint16](preamble_size + sizeof(TaggedValue) * cast[int](function.register_count))

proc get_single_function*(mf: MultiFunction): Function {.inline.} =
  if not mf.is_single_function:
    quit(fmt"The multi-function `{mf.name}` is expected to be a single function.")
  mf.functions[0]

proc instantiate_single_function*(mf: MultiFunction, type_arguments: ImSeq[Type]): ptr FunctionInstance =
  mf.get_single_function.instantiate(type_arguments)

proc instantiate_single_function_unchecked*(mf: MultiFunction, type_arguments: ImSeq[Type]): ptr FunctionInstance =
  let function = mf.get_single_function
  if function.is_monomorphic: addr function.monomorphic_instance
  else: new_function_instance(function, type_arguments)

proc `$`*(function: Function): string =
  # TODO (assembly): Expand this so that the whole function signature is printed. This is crucial for pointing to the
  #                  correct function when the VM errors out, e.g. during dispatch or function instantiation.
  fmt"{function.multi_function.name}"

########################################################################################################################
# Constants.                                                                                                           #
########################################################################################################################

proc new_constants*(): Constants = Constants(types: @[], values: @[], global_variables: @[], multi_functions: @[])
