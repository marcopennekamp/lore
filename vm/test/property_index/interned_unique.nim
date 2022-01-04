discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Names passed to `get_interned_property_index` should be correctly sorted and deduplicated. Once a "normal
# form" has been achieved, the same name sequences must share an interned property index.
block:
  let property_index1 = get_interned_property_index(@["nature", "load", "name", "level"])

  assert find_offset(property_index1, "level") == 0
  assert find_offset(property_index1, "load") == 1
  assert find_offset(property_index1, "name") == 2
  assert find_offset(property_index1, "nature") == 3

  let property_index2 = get_interned_property_index(@["nature", "load", "name", "nature", "level", "load"])

  assert cast[uint](cast[pointer](property_index1)) == cast[uint](cast[pointer](property_index2))
