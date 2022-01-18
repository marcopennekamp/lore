discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Prefix substrings such as "good" vs. "goodwill" should be handled correctly.
block:
  let property_index = get_interned_property_index(@["abc", "abcd", "abcde", "good", "abcf", "abcdf", "abcdu", "abcdefgu", "goodwill"])

  assert property_index.find_offset("abc") == 0
  assert property_index.find_offset("abcd") == 1
  assert property_index.find_offset("abcde") == 2
  assert property_index.find_offset("abcdefgu") == 3
  assert property_index.find_offset("abcdf") == 4
  assert property_index.find_offset("abcdu") == 5
  assert property_index.find_offset("abcf") == 6
  assert property_index.find_offset("good") == 7
  assert property_index.find_offset("goodwill") == 8

  assert property_index.has_property("abc")
  assert property_index.has_property("abcd")
  assert property_index.has_property("abcde")
  assert property_index.has_property("abcdefgu")
  assert property_index.has_property("abcdf")
  assert property_index.has_property("abcdu")
  assert property_index.has_property("abcf")
  assert property_index.has_property("good")
  assert property_index.has_property("goodwill")

  assert not property_index.has_property("abdc")
  assert not property_index.has_property("abcdefguv")
  assert not property_index.has_property("well")
  assert not property_index.has_property("goodwi")
  assert not property_index.has_property("abce")
