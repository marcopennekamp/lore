discard """
 matrix: "--gc:boehm"
"""

import "../../src/imseqs.nim"
import "../../src/types.nim"

# Objective: Ensure that meta shapes are correctly interned.
block:
  let meta_shape1 = get_meta_shape_safe(@["foo"])
  let meta_shape2 = get_meta_shape_safe(@["bar", "baz"])
  let meta_shape3 = get_meta_shape_safe(@["baz"])
  let meta_shape4 = get_meta_shape_safe(@["bar", "baz"])
  let meta_shape5 = get_meta_shape_safe(@["foo"])

  assert meta_shape1 !== meta_shape2
  assert meta_shape1 !== meta_shape3
  assert meta_shape1 !== meta_shape4
  assert meta_shape1 === meta_shape5
  assert meta_shape2 !== meta_shape3
  assert meta_shape2 === meta_shape4
  assert meta_shape2 !== meta_shape5
  assert meta_shape3 !== meta_shape4
  assert meta_shape3 !== meta_shape5
  assert meta_shape4 !== meta_shape5
