discard """
 matrix: "--gc:boehm"
"""

import "../../src/property_index.nim"

# Objective: Prefix substrings such as "good" vs. "goodwill" should be handled correctly.
block:
  let property_index = get_interned_property_index(@["abc", "abcd", "abcde", "good", "abcf", "abcdf", "abcdu", "abcdefgu", "goodwill"])

  assert find_offset(property_index, "abc") == 0
  assert find_offset(property_index, "abcd") == 1
  assert find_offset(property_index, "abcde") == 2
  assert find_offset(property_index, "abcdefgu") == 3
  assert find_offset(property_index, "abcdf") == 4
  assert find_offset(property_index, "abcdu") == 5
  assert find_offset(property_index, "abcf") == 6
  assert find_offset(property_index, "good") == 7
  assert find_offset(property_index, "goodwill") == 8
