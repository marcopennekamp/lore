discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Property indices for a single name are built correctly and lead to the correct result offset.
block:
  let property_index = get_interned_property_index(@["foo"])

  assert property_index.find_offset("foo") == 0
  assert property_index.has_property("foo")
  assert not property_index.has_property("bar")
