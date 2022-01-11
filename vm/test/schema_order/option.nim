discard """
 matrix: "--gc:boehm"
"""

import "../../src/schema_order.nim"

# Objective: Create, sort, and validate a simple Option schema order.
block:
  let graph = new_schema_dependency_graph()
  graph.add_dependency("Game", "Option")
  graph.add_dependency("Some", "Option")
  graph.add_dependency("Player", "Some")
  graph.add_dependency("Game", "Player")
  graph.add_dependency("None", "Option")

  let order = graph.sort_topological()
  assert order == @["Option", "Some", "Player", "Game", "None"]
