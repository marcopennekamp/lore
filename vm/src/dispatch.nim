import std/strformat

from definitions import MultiFunction, FunctionInstance, is_monomorphic
import types
from values import TaggedValue

## Finds the dispatch target for the given multi-function and input type. The function uses an out variable `target` to
## avoid unnecessary allocations of FunctionInstances.
proc find_dispatch_target*(mf: MultiFunction, input_type: TupleType, target: var FunctionInstance) =
  # TODO (vm): This is probably the slowest non-esoteric implementation of dispatch. However, it will suffice for now
  #            until we can spend more time making it fast.
  var candidates = new_seq_of_cap[ptr FunctionInstance](8)
  for function in mf.functions:
    # TODO (vm): This should be the fit, of course. And we should be able to handle polymorphic functions.
    # TODO (vm): This also shouldn't allocate any function instances once we've moved to a hierarchical dispatch
    #            algorithm.
    if is_subtype(input_type, function.input_type):
      assert(function.is_monomorphic)
      candidates.add(addr function.monomorphic_instance)

  # Well, for this to be correct, we also have to filter out all the functions that aren't most specific, because we
  # haven't built a function hierarchy yet. This is very slow, but works.
  var most_specific = new_seq_of_cap[ptr FunctionInstance](8)
  for candidate in candidates:
    var is_most_specific = true
    for f2 in candidates:
      if cast[pointer](candidate) != cast[pointer](f2):
        if is_subtype(f2.function.input_type, candidate.function.input_type):
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

template find_dispatch_target_n(mf, tuple_type, target): untyped =
  # TODO (vm): This optimization is not quite correct. We can assume that the compiler produces valid calls in the
  #            bytecode, but we have to take lower bounds into account.
  # TODO (vm): This is an optimization I would like to be carried out in a preprocessing phase which could for example
  #            replace `dispatch` with `call` instructions.
  let function0 = mf.functions[0]
  if mf.functions.len == 1 and function0.is_monomorphic:
    target = function0.monomorphic_instance
  else:
    find_dispatch_target(mf, tuple_type, target)

proc find_dispatch_target*(mf: MultiFunction, target: var FunctionInstance) =
  find_dispatch_target_n(mf, unit, target)

proc find_dispatch_target*(mf: MultiFunction, argument0: TaggedValue, target: var FunctionInstance) =
  # TODO (vm): We can allocate the tuple type on the stack.
  find_dispatch_target_n(mf, tpl([values.type_of(argument0)]), target)

proc find_dispatch_target*(mf: MultiFunction, argument0: TaggedValue, argument1: TaggedValue, target: var FunctionInstance) =
  # TODO (vm): We can allocate the tuple type on the stack.
  find_dispatch_target_n(mf, tpl([values.type_of(argument0), values.type_of(argument1)]), target)
