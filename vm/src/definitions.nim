import std/hashes
from std/sequtils import any_it
import std/strformat
import std/tables

import imseqs
from instructions import Instruction
from types import TypeParameter, Type, TupleType, FunctionType, MetaShape, Schema, StructSchema, new_function_type,
                  bounds_contain, get_representative, fits_poly1, substitute, `$`
from values import TaggedValue, IntrospectionTypeValue, tag_reference

type
  Frame* = object
    ## A frame represents the memory that the evaluation of a single monomorphic or polymorphic function call requires.
    ## The memory for all frames must be preallocated before the evaluator is invoked.
    ##
    ## Frames are part of `definitions` because some intrinsics require access to frames.
    function_instance*: FunctionInstance
      ## The function instance is embedded in the memory structure of the frame.
    registers*: UncheckedArray[uint64]
      ## Registers may contain TaggedValues and Types. Whether a register currently contains a value or a type is
      ## solely defined by the operations that act on the registers.
  FramePtr* = ptr Frame

  Universe* = ref object
    ## The Universe object provides access to all top-level entities of the current Lore program. The VM can only ever
    ## have one active Universe as it must be accessible as a global variable.
    intrinsics*: TableRef[string, Intrinsic]
    schemas*: TableRef[string, Schema]
    global_variables*: TableRef[string, GlobalVariable]
    multi_functions*: TableRef[string, MultiFunction]
    introspection_type_struct_schema*: StructSchema

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
    dispatch_hierarchy*: DispatchHierarchy
      ## `dispatch_hierarchy` is initialized once all functions associated with the multi-function have been added.

  # TODO (vm): We should keep a list (or sub-hierarchy) of functions with one argument, two arguments, etc., for each
  #            DispatchX operation so that we can quickly access the relevant subset of functions.
  DispatchHierarchy* = distinct ImSeq[DispatchHierarchyNode]
    ## The dispatch hierarchy is defined as the roots of a graph, i.e. `ImSeq[DispatchHierarchyNode]`.

  DispatchHierarchyNode* = ref object
    ## A dispatch hierarchy node represents a function in the dispatch hierarchy and its dispatch successors. Nodes
    ## must be references so that the hierarchy builder can update the ImSeq of successor nodes.
    function*: Function
    # TODO (vm): Do we need the type parameters and input type cache? Are they even beneficial? (This should be
    #            revisited once we have a proper benchmarking solution.)
    type_parameters*: ImSeq[TypeParameter]
      ## This caches the type parameters of the function, removing a layer of pointer indirection.
    input_type*: TupleType
      ## This caches the input type of the function, removing a layer of pointer indirection.
    successors*: ImSeq[DispatchHierarchyNode]

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
    ## A function instance is a function with assigned type arguments. A lambda function instance may additionally have
    ## an associated context if it captures any variables. To avoid allocations on the heap, function instances may
    ## sometimes be placed on the stack or passed by value.
    function*: Function
    type_arguments*: ImSeq[Type]
    lambda_context*: LambdaContext
      ## The lambda context may be `nil` if no variables have been captured. Lambda function instances may or may not
      ## have an associated context. `lambda_context` isn't set during function instantiation, but after a function
      ## instance has already been created.

  LambdaContext* = distinct ImSeq[TaggedValue]
    ## A LambdaContext bundles the values of captured variables for a lambda function.

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
    # TODO (assembly): All kinds of entries in the constants table are 8 bytes wide (we should be careful with strings,
    # though). To remove a layer of pointer indirection, we could remove the individual sequences and turn `Constants`
    # into a single `ImSeq`. This would require a new limit to be introduced for the maximum number of constants, or
    # alternatively constant IDs within a function would become "global" across all kinds of constants, which is likely
    # the preferred way. This would play well with having one `Constants` instance per `Function`, which is another
    # outstanding issue. If an entry isn't 8 bytes wide, we can always put it in a `ref object`, which will add pointer
    # indirection for that specific kind of entry, but this is still preferable to having pointer indirection for ALL
    # kinds of entries.
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

const introspection_type_trait_name*: string = "lore.core.Type"
  ## The VM needs to know the Type trait as defined in `core.lore` to properly "generate" a backing struct for it.

proc `===`*(f1: Function, f2: Function): bool
proc `!==`*(f1: Function, f2: Function): bool = not (f1 === f2)

proc `$`*(function: Function): string

########################################################################################################################
# Universes.                                                                                                           #
########################################################################################################################

# TODO (vm/parallel): Make sure that `active_universe` is thread-safe. (It should be, out of the box, as the universe
#                     won't be mutated after it's been created, but there might be things to look out for.)
var active_universe: Universe = nil
  ## This is the active universe which can be accessed as a global variable from across the VM.

proc get_active_universe*(): Universe =
  if active_universe == nil:
    quit("Cannot get the active universe, as it is `nil`.")
  active_universe

proc set_active_universe*(universe: Universe) =
  active_universe = universe

proc new_introspection_type_value*(universe: Universe, boxed_type: Type): IntrospectionTypeValue =
  IntrospectionTypeValue(tpe: universe.introspection_type_struct_schema.get_representative, boxed_type: boxed_type)

########################################################################################################################
# Frames.                                                                                                              #
########################################################################################################################

# These accessors shouldn't produce any overhead because `function_instance` is embedded into `frame`.
proc function*(frame: FramePtr): Function {.inline.} = frame.function_instance.function
proc type_arguments*(frame: FramePtr): ImSeq[Type] {.inline.} = frame.function_instance.type_arguments
proc lambda_context*(frame: FramePtr): LambdaContext {.inline.} = frame.function_instance.lambda_context

########################################################################################################################
# Global variables.                                                                                                    #
########################################################################################################################

proc new_eager_global*(name: string, value: TaggedValue): GlobalVariable =
  GlobalVariable(name: name, value: value, is_initialized: true)

proc new_lazy_global*(name: string, initializer: FunctionInstance): GlobalVariable =
  GlobalVariable(name: name, value: tag_reference(nil), is_initialized: false, initializer: initializer)

########################################################################################################################
# Multi-functions.                                                                                                     #
########################################################################################################################

proc are_functions_unique*(mf: MultiFunction): bool =
  ## Whether no function pairs in `mf` are equally specific.
  for f1 in mf.functions:
    let has_duplicate = mf.functions.any_it(
      f1 !== it and
        fits_poly1(f1.input_type, it.input_type, it.type_parameters) != nil and
        fits_poly1(it.input_type, f1.input_type, f1.type_parameters) != nil
    )
    if has_duplicate:
      return false
  true

########################################################################################################################
# Dispatch hierarchies.                                                                                                #
########################################################################################################################

proc roots*(dispatch_hierarchy: DispatchHierarchy): ImSeq[DispatchHierarchyNode] {.inline.} = cast[ImSeq[DispatchHierarchyNode]](dispatch_hierarchy)

proc `===`*(n1: DispatchHierarchyNode, n2: DispatchHierarchyNode): bool =
  ## Checks the referential equality of the two dispatch hierarchy nodes.
  cast[pointer](n1) == cast[pointer](n2)

proc `!==`*(n1: DispatchHierarchyNode, n2: DispatchHierarchyNode): bool = not (n1 === n2)

proc attach_dispatch_hierarchy*(mf: MultiFunction, dispatch_hierarchy: DispatchHierarchy) =
  ## Attaches the given hierarchy to the multi-function.
  if cast[ImSeq[DispatchHierarchyNode]](mf.dispatch_hierarchy) != nil:
    quit(fmt"Cannot attach a dispatch hierarchy to the multi-function `{mf.name}` as it already has a dispatch hierarchy.")
  mf.dispatch_hierarchy = dispatch_hierarchy

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

proc new_function_instance*(): ptr FunctionInstance =
  ## Allocates an uninitialized function instance. The fields `function` and `type_arguments` must be initialized
  ## manually.
  new_function_instance(nil, nil)

proc can_instantiate_polymorphic(function: Function, type_arguments: ImSeq[Type]): bool {.inline.} =
  ## Whether `function` can be instantiated with the given type arguments. Assumes that `function` is polymorphic.
  let type_parameters = function.type_parameters
  for i in 0 ..< type_parameters.len:
    if not bounds_contain(type_parameters[i], type_arguments[i], type_arguments.to_open_array):
      return false
  true

proc fail_instantiation(function: Function, type_arguments: ImSeq[Type]) {.inline.} =
  quit(fmt"Cannot instantiate function `{function}` with type arguments `{type_arguments}`.")

proc instantiate*(function: Function, type_arguments: ImSeq[Type]): ptr FunctionInstance =
  # TODO (vm): When using this function, we technically only need to check lower bounds of covariant parameters and
  #            upper bounds of contravariant parameters, as we can reasonably assume that the compiler has covered the
  #            other direction. This could be an optimization specifically for instantiations from instructions such
  #            as `CallPoly`.
  if function.is_monomorphic: addr function.monomorphic_instance
  else:
    if unlikely(not can_instantiate_polymorphic(function, type_arguments)):
      fail_instantiation(function, type_arguments)
    new_function_instance(function, type_arguments)

proc ensure_can_instantiate*(function: Function, type_arguments: ImSeq[Type]) =
  ## Quits execution if `function` cannot be instantiated with `type_arguments`. Applies the same checks to the type
  ## arguments as `instantiate`. This function is meant to be used in cases where a function instance is not needed.
  ## It's not meant to be used as a check before calling `instantiate`.
  if unlikely(function.is_polymorphic and not can_instantiate_polymorphic(function, type_arguments)):
    fail_instantiation(function, type_arguments)

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

proc `===`*(f1: Function, f2: Function): bool =
  ## Checks the referential equality of the two functions.
  cast[pointer](f1) == cast[pointer](f2)

# Function equality/hashing is defined as referential equality/hashing and used for including functions in hash sets.
proc `==`*(f1: Function, f2: Function): bool = f1 === f2
proc hash*(function: Function): Hash = hash(cast[pointer](function))

proc `$`*(function: Function): string = fmt"{function.multi_function.name}{function.input_type}: {function.output_type}"

proc `$`*(instance: ptr FunctionInstance): string =
  let function_type = instance.get_function_type()
  fmt"{instance.function.multi_function.name}{function_type.input}: {function_type.output}"

proc `[]`*(context: LambdaContext, index: int): TaggedValue {.borrow.}
proc `[]`*(context: LambdaContext, index: int64): TaggedValue {.borrow.}
proc `[]`*(context: LambdaContext, index: uint): TaggedValue {.borrow.}

########################################################################################################################
# Constants.                                                                                                           #
########################################################################################################################

proc new_constants*(): Constants = Constants(types: @[], values: @[], global_variables: @[], multi_functions: @[])
