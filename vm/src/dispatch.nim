from std/sequtils import all_it, any_it
import std/sets
import std/strformat

import definitions
import imseqs
import types
import values

########################################################################################################################
# Min sets.                                                                                                            #
########################################################################################################################

type MinCandidate = object
  node: DispatchHierarchyNode
  type_arguments: ImSeq[Type]

iterator min(hierarchy: DispatchHierarchy, input_types: open_array[Type]): FunctionInstance =
  ## This iterator goes through all FunctionInstances in the min set of dispatch given `input_types`. The function
  ## instances are yielded by value. The same function may be yielded multiple times if multiple paths lead to it, so
  ## uniqueness must be enforced by the user of this iterator, if so desired. `input_types` may not contain any type
  ## variables.
  # The `remaining` set contains only those nodes for which `input_types fits node.input_type` has already been proven.
  # This allows us to have a single fits check per visited node, instead of one fits check for "visiting" a node and
  # one fits check for "selecting" a node (i.e. making sure that none of the node's successors are in the fit as well,
  # which proves the node to be in the min set). `MinCandidate` exists to carry the `type_arguments` forward, as they
  # are calculated by `fits`, and are needed in case the node is selected.
  # TODO (vm): Can we get rid of the allocation here? Maybe a sequence that is constantly reused? It has to be
  #            benchmarked, of course.
  var remaining = new_seq_of_cap[MinCandidate](8)
  for node in hierarchy.roots:
    let type_arguments = fits(input_types, node.input_type.elements.to_open_array, node.type_parameters)
    if type_arguments != nil:
      remaining.add(MinCandidate(node: node, type_arguments: type_arguments))

  while remaining.len > 0:
    let candidate = remaining.pop()

    # The node will be selected for the min set if none of its children are in the fit. Otherwise, one or more children
    # are added to `remaining` and `node` won't be yielded as a result.
    var should_select = true
    for successor in candidate.node.successors:
      let type_arguments = fits(input_types, successor.input_type.elements.to_open_array, successor.type_parameters)
      if type_arguments != nil:
        should_select = false
        remaining.add(MinCandidate(node: successor, type_arguments: type_arguments))

    if should_select:
      yield FunctionInstance(function: candidate.node.function, type_arguments: candidate.type_arguments)

proc min_poly_node(roots: open_array[DispatchHierarchyNode], input_types: open_array[Type]): seq[DispatchHierarchyNode] =
  ## `min_poly_node` is used to build the dispatch hierarchy. It works like `min`, but the function doesn't require a
  ## dispatch hierarchy instance, `input_types` may contain type variables, and it returns dispatch hierarchy nodes
  ## directly. Dispatch hierarchy nodes are guaranteed to be unique, in contrast to `min`.
  # The algorithm varies from `min` in the following ways:
  #   - Input types may contain type variables, so the algorithm uses `fits_poly1`.
  #   - The function returns a list of unique dispatch hierarchy nodes, which are needed by the hierarchy builder. It
  #     cannot be an iterator because the iterator shouldn't be affected by changes to the roots and other nodes as it
  #     iterates.
  #   - As the function doesn't return FunctionInstances, there is no need to keep the type arguments, so the
  #     `remaining` list can be simplified.
  var remaining = new_seq_of_cap[DispatchHierarchyNode](8)
  for node in roots:
    if fits_poly1(input_types, node.input_type.elements.to_open_array, node.type_parameters) != nil:
      remaining.add(node)

  var results = new_seq_of_cap[DispatchHierarchyNode](4)
  while remaining.len > 0:
    let node = remaining.pop()

    var should_select = true
    for successor in node.successors:
      if fits_poly1(input_types, successor.input_type.elements.to_open_array, successor.type_parameters) != nil:
        should_select = false
        remaining.add(successor)

    if should_select and results.all_it(it !== node):
      results.add(node)
  results

########################################################################################################################
# Dispatch target querying.                                                                                            #
########################################################################################################################

const use_dispatch_hierarchy = true

proc find_dispatch_target*(mf: MultiFunction, input_types: open_array[Type], target: var FunctionInstance) =
  ## Finds the dispatch target for the given multi-function and input types (expressed as an open array of tuple
  ## elements). The function uses an out variable `target` to avoid unnecessary allocations of FunctionInstances.
  # TODO (vm): This is an optimization I would like to be carried out during poem instruction resolution which could
  #            replace `dispatch` with `call` instructions.
  let function0 = mf.functions[0]
  if mf.functions.len == 1 and function0.is_monomorphic:
    target = function0.monomorphic_instance
    return

  when use_dispatch_hierarchy:
    var has_found_target = false
    for function_instance in min(mf.dispatch_hierarchy, input_types):
      if likely(not has_found_target):
        has_found_target = true
        target = function_instance
      elif target.function !== function_instance.function:
        # Even if we already have found a target, `min` doesn't guarantee that function instances are unique, so we
        # have to additionally check that the existing target and the new function instance are actually two different
        # functions to prove ambiguity.
         quit(fmt"Cannot call multi-function {mf.name} given input types `{input_types}`: ambiguous call. Conflicting functions:{'\n'}{target} and {function_instance}.")

    if unlikely(not has_found_target):
      quit(fmt"Cannot call multi-function {mf.name} given input types `{input_types}`: empty fit.")
    elif unlikely(target.function.is_abstract):
      quit(fmt"Cannot call multi-function {mf.name} given input types `{input_types}`: the chosen target function `{target.function}` is abstract.")
  else:
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

    if unlikely(most_specific.len == 0):
      quit(fmt"Cannot call multi-function {mf.name} given input types `{input_types}`: empty fit.")
    elif unlikely(most_specific.len > 1):
      let signature_strings = new_immutable_seq(candidates).join("\n")
      quit(fmt"Cannot call multi-function {mf.name} given input types `{input_types}`: ambiguous call. Candidates:{'\n'}{signature_strings}.")

    target = most_specific[0][]
    if target.function.is_abstract:
      quit(fmt"Cannot call multi-function {mf.name}: the chosen target function `{target.function}` is abstract.")

proc find_dispatch_target_from_arguments*(mf: MultiFunction, target: var FunctionInstance) =
  find_dispatch_target(mf, [], target)

proc find_dispatch_target_from_arguments*(mf: MultiFunction, argument0: TaggedValue, target: var FunctionInstance) =
  find_dispatch_target(mf, [argument0.get_type], target)

proc find_dispatch_target_from_arguments*(mf: MultiFunction, argument0: TaggedValue, argument1: TaggedValue, target: var FunctionInstance) =
  find_dispatch_target(mf, [argument0.get_type, argument1.get_type], target)

proc find_dispatch_target_from_arguments*(mf: MultiFunction, arguments: open_array[TaggedValue], target: var FunctionInstance) =
  # TODO (vm): We could avoid an allocation here for e.g. 16 or less arguments if we allocate the array on the stack.
  #            Alternatively, we can keep a global sequence and then worry about threading later.
  var input_types = new_seq[Type](arguments.len)
  for i in 0 ..< input_types.len:
    input_types[i] = arguments[i].get_type
  find_dispatch_target(mf, input_types, target)

########################################################################################################################
# Dispatch hierarchy building.                                                                                         #
########################################################################################################################

proc build_dispatch_hierarchy_node(function: Function): DispatchHierarchyNode

proc build_dispatch_hierarchy*(mf: MultiFunction): DispatchHierarchy =
  ## Builds the dispatch hierarchy for the given multi-function. `build_dispatch_hierarchy` should only be called once
  ## all functions have been added to the multi-function.
  ##
  ## Note that this algorithm assumes that there is no pair of functions which are equally specific. This case should
  ## be handled during universe resolution, which must guarantee uniqueness.
  ##
  ## Also see the corresponding `DispatchHierarchyBuilder` of the compiler. The algorithm is the same and better
  ## commented in the Scala version.
  var roots = new_seq_of_cap[DispatchHierarchyNode](5)
  var remaining = mf.functions.to_hash_set
  var top = new_seq_of_cap[Function](16) # `top` is reused for each iteration of the loop to avoid reallocations.

  while remaining.len > 0:
    for f1 in remaining:
      let has_superfunction = remaining.any_it(
        f1 !== it and
          fits_poly1(f1.input_type.elements.to_open_array, it.input_type.elements.to_open_array, it.type_parameters) != nil
      )
      if not has_superfunction:
        top.add(f1)

    if unlikely(top.len == 0):
      quit(fmt"Cannot build a dispatch hierarchy for multi-function {mf.name}: the list of top functions is empty.")

    for f1 in top:
      let min_set = min_poly_node(roots, f1.input_type.elements.to_open_array)
      if min_set.len > 0:
        for node in min_set:
          let successor = build_dispatch_hierarchy_node(f1)
          node.successors = node.successors.append(successor)
      else:
        # If the min set is empty, there is currently no function in the hierarchy which would be a superfunction of
        # `f1`, so we have to add `f1` as a root.
        roots.add(build_dispatch_hierarchy_node(f1))
      remaining.excl(f1)

    # We have to reset `top` as it'll be reused in the next iteration.
    top.set_len(0)

  DispatchHierarchy(new_immutable_seq(roots))

proc build_dispatch_hierarchy_node(function: Function): DispatchHierarchyNode =
  DispatchHierarchyNode(
    function: function,
    type_parameters: function.type_parameters,
    input_type: function.input_type,
    successors: empty_immutable_seq[DispatchHierarchyNode](),
  )
