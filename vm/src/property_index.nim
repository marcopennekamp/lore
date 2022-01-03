type
  PropertyIndex* = ref object
    ## A property index maps a predetermined set of names to a contiguous range of property offsets. This is used to
    ## store property types and values in an array, by accessing them via their offsets. Names are indexed in
    ## lexicographic order.
    ##
    ## Property indices should be precomputed for shapes and structs, as their set of property names is always known
    ## before evaluation. Property indices are also interned, so that the same name set found in different struct and
    ## shape types can be shared in memory.
    ##
    ## Due to how property indices are optimized, they cannot be used to decide whether a property name is valid.
    ## Asking a property index for the offset of a name which isn't valid is undefined behavior (including the
    ## possibility of a segfault).
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
    ##       - e --> result 0
    ##       - o --> result 1
    ##     - n --> node { position: 2 }
    ##       - m --> result 2
    ##       - t --> result 3
    ##
    ## Notice how the index doesn't fully include the strings. This is intentional, because we can reasonably expect
    ## that the index is only queried with valid property names. The Lore compiler can already guarantee this. This
    ## keeps the structure light and fast to traverse.
    position: uint16
    edge_count: uint16
    edges: UncheckedArray[IndexEdge]

  IndexEdge = object
    ## An index edge points to a `target` or to a `result` given a `significant` byte.
    significant: char
    case is_result: bool
    of true:
      result: uint16
    of false:
      target: IndexNode

proc alloc_index_node(position: uint16, edge_count: uint16): IndexNode =
  ## Allocates an IndexNode with space reserved for `edge_count` edges.
  let node = cast[IndexNode](alloc0(sizeof(IndexNode) + cast[int](edge_count) * sizeof(IndexEdge)))
  node.position = position
  node.edge_count = edge_count
  node

proc new_result_edge(significant: char, res: uint16): IndexEdge {.inline.} =
  IndexEdge(significant: significant, is_result: true, result: res)

proc new_branch_edge(significant: char, target: IndexNode): IndexEdge {.inline.} =
  IndexEdge(significant: significant, is_result: false, target: target)

proc find_offset*(property_index: PropertyIndex, name: open_array[char]): uint16 =
  ## In the given PropertyIndex, finds the offset for `name`. This operation is undefined if `name` is not in the
  ## property index (including the possibility of a segfault).
  var current_node: IndexNode = property_index.root
  while true:
    let significant = name[current_node.position]
    var i = 0'u16
    while i < current_node.edge_count:
      let edge = current_node.edges[i]
      if edge.significant == significant:
        if edge.is_result:
          return edge.result
        else:
          current_node = edge.target
          break
      i += 1

    # No edge corresponds to the current `significant`: the name isn't included in the property index. We return 0 as a
    # default result. This is technically undefined behavior.
    if i == current_node.edge_count:
      return 0

proc test_property_index() =
  ## Tests a property index for strings "level", "load", "name", and "nature".
  let l0 = alloc_index_node(1, 2)
  l0.edges[0] = new_result_edge('e', 0)
  l0.edges[1] = new_result_edge('o', 1)

  let n0 = alloc_index_node(2, 2)
  n0.edges[0] = new_result_edge('m', 2)
  n0.edges[1] = new_result_edge('t', 3)

  let root = alloc_index_node(0, 2)
  root.edges[0] = new_branch_edge('l', l0)
  root.edges[1] = new_branch_edge('n', n0)

  let property_index = PropertyIndex(root: root)

  echo get_index(property_index, "level")
  echo get_index(property_index, "load")
  echo get_index(property_index, "name")
  echo get_index(property_index, "nature")

when is_main_module:
  test_property_index()
