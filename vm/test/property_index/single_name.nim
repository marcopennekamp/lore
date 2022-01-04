discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Property indices for a single name are built correctly and lead to the correct result offset.
block:
  let property_index = get_interned_property_index(@["foo"])

  assert find_offset(property_index, "foo") == 0
