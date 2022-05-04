discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Names passed to `get_interned_property_index` should be correctly sorted and deduplicated. Once a "normal
# form" has been achieved, the same name sequences must share an interned property index.
block:
  let property_index1 = get_interned_property_index(@["nature", "load", "name", "level"])

  assert property_index1.find_offset("level") == 0
  assert property_index1.find_offset("load") == 1
  assert property_index1.find_offset("name") == 2
  assert property_index1.find_offset("nature") == 3

  assert property_index1.has_property("level")
  assert property_index1.has_property("load")
  assert property_index1.has_property("name")
  assert property_index1.has_property("nature")

  assert not property_index1.has_property("function")
  assert not property_index1.has_property("levele")
  assert not property_index1.has_property("looad")
  assert not property_index1.has_property("nname")
  assert not property_index1.has_property("nature?")

  let property_index2 = get_interned_property_index(@["nature", "load", "name", "nature", "level", "load"])

  assert cast[pointer](property_index1) == cast[pointer](property_index2)
