from std/algorithm import sort
from std/sequtils import deduplicate, to_seq
import tables

type
  PropertyIndex* = ref object
    ## A property index maps a predetermined set of names to a contiguous range of property offsets. This is used to
    ## store property types and values in an array, by accessing them via their offsets. Names are indexed in
    ## lexicographic order.
    ##
    ## Property indices are precomputed for shapes and structs, as their set of property names is always known before
    ## evaluation. Property indices are also interned, so that the same name set found in different struct and shape
    ## types can be shared in memory.
    ##
    ## The `find_offset` operation cannot be used to decide whether a property name is valid. This is due to how the
    ## operation is optimized. Asking a property index for the offset of a name which isn't valid is undefined
    ## behavior. The `has_property` operation should be used instead.
    root: IndexNode

  IndexNode = ref object
    ## The internal representation of a property index consists of a series of nodes which decide a path to follow for
    ## a given string. Each node exploits a critical difference between two or more strings in a single byte at some
    ## byte `position`.
    ##
    ## For example, the property index for the names "nature", "name", "level", and "load" would look like this:
    ##
    ##   - root { position: 0 }
    ##     - l --> node { position: 1 }
    ##       - e --> result 0 ("level")
    ##       - o --> result 1 ("load")
    ##     - n --> node { position: 2 }
    ##       - m --> result 2 ("name")
    ##       - t --> result 3 ("nature")
    ##
    ## To find an offset, we can follow the index structure to the result offset, which is a fast operation. Comparing
    ## names is only necessary to decide `has_property`. Many times, the bytecode already guarantees by contract that a
    ## name is valid, so `has_property` only needs to be used in some cases.
    position: uint16
    edge_count: uint16
    edges: UncheckedArray[IndexEdge]

  IndexEdge = object
    ## An index edge points to a `target` or to a `result` given a `significant` byte.
    significant: char
    case is_result: bool
    of true:
      result: uint16
      name: string
        ## The full name of the result property. This is used by `has_property` to determine whether a given name is
        ## contained in the property index.
    of false:
      target: IndexNode
  IndexEdgePtr = ptr IndexEdge

proc significant_at(name: open_array[char], position: uint16): char =
  ## Returns the significant at `position` in `name`. If `position` is out of bounds, the null character `\0` is the
  ## significant. This allows the build and find algorithms to properly process name sets such as "good" and
  ## "goodwill", where one name is the prefix of another.
  if cast[int](position) < name.len:
    name[position]
  else:
    '\0'

proc build_property_index(names: seq[string]): PropertyIndex

########################################################################################################################
# Index API.                                                                                                           #
########################################################################################################################

proc find_result_edge(property_index: PropertyIndex, name: open_array[char]): IndexEdgePtr {.inline.} =
  ## Finds a result edge in the given property index for the given name. `nil` may be returned if `name` is not in the
  ## property index.
  var current_node: IndexNode = property_index.root
  while true:
    let significant = name.significant_at(current_node.position)
    var i = 0'u16
    while i < current_node.edge_count:
      let edge = current_node.edges[i]
      if edge.significant == significant:
        if edge.is_result:
          return addr current_node.edges[i]
        else:
          current_node = edge.target
          break
      i += 1

    # No edge corresponds to the current `significant`: the name isn't included in the property index.
    if i == current_node.edge_count:
      return nil

proc find_offset*(property_index: PropertyIndex, name: open_array[char]): uint16 =
  ## In the given property index, finds the offset for `name`. This operation is undefined if `name` is not in the
  ## property index.
  let result_edge = find_result_edge(property_index, name)
  if result_edge != nil:
    result_edge.result
  else:
    # We return 0 as a default offset. This is technically undefined behavior.
    0

proc has_property*(property_index: PropertyIndex, name: open_array[char]): bool =
  ## Whether `name` is in the given property index.
  let result_edge = find_result_edge(property_index, name)
  result_edge != nil and result_edge.name == name

# TODO (vm/parallel): This should be protected by a lock.
var interned_property_indices = new_table[seq[string], PropertyIndex]()

proc get_interned_property_index*(names: open_array[string]): PropertyIndex =
  ## Returns a property index for the given names if it's already been interned. Otherwise, creates such a property
  ## index and caches it.
  var sorted_names = to_seq(names)
  sort(sorted_names)
  let unique_names = deduplicate(sorted_names, is_sorted = true)
  var property_index = interned_property_indices.get_or_default(unique_names)
  if property_index == nil:
    property_index = build_property_index(unique_names)
    interned_property_indices[unique_names] = property_index
  property_index

########################################################################################################################
# Index building.                                                                                                      #
########################################################################################################################

proc alloc_index_node(position: uint16, edge_count: uint16): IndexNode =
  ## Allocates an IndexNode with space reserved for `edge_count` edges.
  let node = cast[IndexNode](alloc0(sizeof(IndexNode) + cast[int](edge_count) * sizeof(IndexEdge)))
  node.position = position
  node.edge_count = edge_count
  node

proc new_result_edge(significant: char, res: uint16, name: string): IndexEdge {.inline.} =
  IndexEdge(significant: significant, is_result: true, result: res, name: name)

proc new_branch_edge(significant: char, target: IndexNode): IndexEdge {.inline.} =
  IndexEdge(significant: significant, is_result: false, target: target)

proc count_unique_significants(position: uint16, names: open_array[string], first: uint16, last: uint16): uint16 =
  ## Counts the number of unique significants at the given position in `names`, which must be sorted lexicographically.
  if first == last:
    return 0'u16

  var count = 1'u16
  for i in first + 1 .. last:
    if names[i - 1].significant_at(position) != names[i].significant_at(position):
      count += 1
  count

proc build_index_node(start_position: uint16, names: open_array[string], first: uint16, last: uint16): IndexNode =
  ## Builds an index node from the sorted list of names starting at index `first` and ending at index `last`, with this
  ## node's critical position possibly being `start_position` or a subsequent position. `names` isn't presented as a
  ## slice so that we can assign the correct global index to a result edge.

  # First we have to find a position at which two or more of the names differ.
  var critical_position = start_position
  var unique_significants_count = count_unique_significants(critical_position, names, first, last)
  while unique_significants_count < 2:
    critical_position += 1
    unique_significants_count = count_unique_significants(critical_position, names, first, last)

  # The number of edges of the node is the number of unique significants, as we need exactly this many edges to
  # differentiate all possible significant differences.
  let node = alloc_index_node(critical_position, unique_significants_count)
  var next_free_index = first
  for edge_index in 0'u16 ..< unique_significants_count:
    assert next_free_index <= last

    # For each edge, we have to find its first and last name index. This is basically a reiteration of
    # `count_unique_significants`.
    let edge_first = next_free_index
    var edge_last = edge_first
    while edge_last < last and names[edge_last].significant_at(critical_position) == names[edge_last + 1].significant_at(critical_position):
      edge_last += 1

    let name = names[edge_first]
    let significant = name.significant_at(critical_position)
    let edge =
      if edge_first == edge_last:
        # We have a single name, hence a result edge!
        new_result_edge(significant, edge_first, name)
      else:
        let branch_node = build_index_node(critical_position + 1, names, edge_first, edge_last)
        new_branch_edge(significant, branch_node)
    node.edges[edge_index] = edge

    next_free_index = edge_last + 1
  node

proc build_property_index(names: seq[string]): PropertyIndex =
  ## Builds a property index for the given `names`, which must be sorted and unique. This algorithm recursively builds
  ## nodes by taking subranges of `names` with the same critical byte into `build_index_node`. For example, for the
  ## "level" to "nature" example, the call tree will look like this:
  ##
  ##  - build_index_node(0, ["level", "load", "name", "nature"], 0, 3)
  ##    - build_index_node(1, ["level", "load", "name", "nature"], 0, 1)
  ##    - build_index_node(1, ["level", "load", "name", "nature"], 2, 3)
  ##
  ## When `build_index_node` is called, the algorithm finds the first position after `start_position` at which some of
  ## the `names` differ. At that position, `names` is divided into a number of segments, for which each
  ## `build_index_node` is called once, creating a target edge. If a segment only has a single name, a result edge is
  ## produced instead.
  let root =
    if names.len == 0:
      alloc_index_node(0, 0)
    elif names.len == 1:
      # We handle this edge case separately, because there is no need to call `build_index_node`.
      let name = names[0]
      let root = alloc_index_node(0, 1)
      root.edges[0] = new_result_edge(name.significant_at(0), 0, name)
      root
    else:
      build_index_node(0, names, 0, uint16(names.len - 1))

  PropertyIndex(root: root)
