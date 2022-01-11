import std/sets
import tables

type
  SchemaDependencyGraph* = ref object
    ## The schema dependency graph allows computing the proper resolution order for schemas. It is represented by an
    ## adjacency list from a dependency name to a set of dependant names. For example, the dependency graph for options
    ## would look like this:
    ##
    ##  - "Option" --> "Some", "None"
    ##
    ## The schema resolution order is computed using a topological sort.
    table: TableRef[string, HashSet[string]]

proc new_schema_dependency_graph*(): SchemaDependencyGraph = SchemaDependencyGraph(table: new_table[string, HashSet[string]]())

proc add_dependency*(graph: SchemaDependencyGraph, dependant: string, dependency: string) =
  discard graph.table.mget_or_put(dependant, init_hash_set[string]())
  graph.table.mget_or_put(dependency, init_hash_set[string]()).incl(dependant)

proc sort_topological*(graph: SchemaDependencyGraph): seq[string] =
  let table = graph.table
  var order = new_seq[string]()
  var queue = new_seq[string]()
  var in_degree = new_table[string, int]()

  # Collect the correct number of incoming edges for each node. `mget_or_put` allows the algorithm to avoid doubled key
  # accesses when checking for "has key" before incrementing an in-degree, for example.
  for name, dependants in table.pairs():
    discard in_degree.mget_or_put(name, 0)
    for dependant in dependants:
      inc(in_degree.mget_or_put(dependant, 0))

  for name in table.keys():
    if in_degree[name] == 0:
      queue.add(name)

  while queue.len > 0:
    let candidate = queue.pop()
    order.add(candidate)
    for dependant in table[candidate]:
      let d = in_degree[dependant] - 1
      in_degree[dependant] = d
      if d == 0:
        queue.add(dependant)

  # If the lengths don't match, there is at least one node which couldn't be added to the queue. This proves that the
  # graph has a cycle, for which topological sort is not defined.
  if order.len != in_degree.len:
    raise new_exception(
      ValueError,
      "The schema dependency graph has a cycle and thus cannot be sorted. A Lore compiler should not produce" &
        " bytecode with dependency cycles between schemas!",
    )

  order
