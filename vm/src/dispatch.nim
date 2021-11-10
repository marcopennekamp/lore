import std/strformat

import functions
import types
from values import TaggedValue

proc get_dispatch_target*(mf: MultiFunction, input_type: TupleType): Function =
  # TODO (vm): This is probably the slowest non-esoteric implementation of dispatch. However, it will suffice for now
  #            until we can spend more time making it fast.
  var candidates = new_seq_of_cap[Function](8)
  for function in mf.functions:
    # TODO (vm): This should be the fit, of course.
    if is_subtype(input_type, function.input_type):
      candidates.add(function)

  # Well, for this to be correct, we also have to filter out all the functions that aren't most specific, because we
  # haven't built a function hierarchy yet. This is very slow, but works.
  var most_specific = new_seq_of_cap[Function](8)
  for candidate in candidates:
    var is_most_specific = true
    for f2 in candidates:
      if cast[pointer](candidate) != cast[pointer](f2):
        if is_subtype(f2.input_type, candidate.input_type):
          is_most_specific = false
          break
    if is_most_specific: most_specific.add(candidate)

  # TODO (vm): Improve these error messages by including the input type and the candidates (if ambiguous).
  if most_specific.len == 0:
    quit(fmt"Cannot call multi-function {mf.name}: empty fit.")
  elif most_specific.len > 1:
    quit(fmt"Cannot call multi-function {mf.name}: ambiguous call.")

  let target = most_specific[0]
  if target.is_abstract:
    # TODO (vm): Specify the exact offending function.
    quit(fmt"Cannot call multi-function {mf.name}: the chosen target function is abstract.")
  target

template get_dispatch_target_fixed(mf, tuple_type): untyped =
  # TODO (vm): This optimization is not quite correct. We can assume that the compiler produces valid calls in the
  #            bytecode, but we have to take lower bounds into account.
  # TODO (vm): This is an optimization I would like to be carried out in a preprocessing phase which could for example
  #            replace `dispatch` with `call` instructions.
  if mf.functions.len == 1:
    return mf.functions[0]
  else:
    get_dispatch_target(mf, tuple_type)

proc get_dispatch_target*(mf: MultiFunction, argument0: TaggedValue): Function =
  # TODO (vm): We can technically omit creating the tuple if we're a little smart about it. But this is an
  #            optimization for later.
  get_dispatch_target_fixed(mf, tpl([values.type_of(argument0)]))

proc get_dispatch_target*(mf: MultiFunction, argument0: TaggedValue, argument1: TaggedValue): Function =
  get_dispatch_target_fixed(mf, tpl([values.type_of(argument0), values.type_of(argument1)]))
