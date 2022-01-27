import std/strformat

from definitions import MultiFunction, FunctionInstance, is_monomorphic, new_function_instance
import imseqs
import types
from values import TaggedValue

proc find_dispatch_target*(mf: MultiFunction, input_types: open_array[Type], target: var FunctionInstance) =
  ## Finds the dispatch target for the given multi-function and input types (expressed as an open array of tuple
  ## elements). The function uses an out variable `target` to avoid unnecessary allocations of FunctionInstances.
  # TODO (vm): This is probably the slowest non-esoteric implementation of dispatch. However, it will suffice for now
  #            until we can spend more time making it fast.
  var candidates = new_seq_of_cap[ptr FunctionInstance](8)
  for function in mf.functions:
    let function_input_types = function.input_type.elements
    if function.is_monomorphic:
      if is_subtype(input_types, function_input_types.to_open_array):
        candidates.add(addr function.monomorphic_instance)
    else:
      let type_arguments = fits(input_types, function_input_types.to_open_array, function.type_parameters)
      if type_arguments != nil:
        # TODO (vm): This shouldn't need to allocate any function instances once we've moved to hierarchical dispatch.
        candidates.add(new_function_instance(function, type_arguments))

  # Well, for this to be correct, we also have to filter out all the functions that aren't most specific, because we
  # haven't built a function hierarchy yet. This is very slow, but works.
  var most_specific = new_seq_of_cap[ptr FunctionInstance](8)
  for candidate in candidates:
    var is_most_specific = true
    for f2 in candidates:
      if cast[pointer](candidate) != cast[pointer](f2):
        let type_arguments = fits_poly1(
          to_open_array(f2.function.input_type.elements),
          to_open_array(candidate.function.input_type.elements),
          candidate.function.type_parameters,
        )
        if type_arguments != nil:
          is_most_specific = false
          break
    if is_most_specific: most_specific.add(candidate)

  # TODO (vm): Improve these error messages by including the input type and the candidates (if ambiguous).
  if most_specific.len == 0:
    quit(fmt"Cannot call multi-function {mf.name}: empty fit.")
  elif most_specific.len > 1:
    quit(fmt"Cannot call multi-function {mf.name}: ambiguous call.")

  target = most_specific[0][]
  if target.function.is_abstract:
    # TODO (vm): Specify the exact offending function.
    quit(fmt"Cannot call multi-function {mf.name}: the chosen target function is abstract.")

template find_dispatch_target_n(mf, input_types, target): untyped =
  # TODO (vm): This is an optimization I would like to be carried out in a preprocessing phase which could for example
  #            replace `dispatch` with `call` instructions.
  let function0 = mf.functions[0]
  if mf.functions.len == 1 and function0.is_monomorphic:
    target = function0.monomorphic_instance
  else:
    find_dispatch_target(mf, input_types, target)

proc find_dispatch_target*(mf: MultiFunction, target: var FunctionInstance) =
  find_dispatch_target_n(mf, [], target)

proc find_dispatch_target*(mf: MultiFunction, argument0: TaggedValue, target: var FunctionInstance) =
  # TODO (vm): We can allocate the tuple type on the stack.
  find_dispatch_target_n(mf, [values.get_type(argument0)], target)

proc find_dispatch_target*(mf: MultiFunction, argument0: TaggedValue, argument1: TaggedValue, target: var FunctionInstance) =
  # TODO (vm): We can allocate the tuple type on the stack.
  find_dispatch_target_n(mf, [values.get_type(argument0), values.get_type(argument1)], target)
