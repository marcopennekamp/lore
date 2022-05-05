discard """
 matrix: "--gc:boehm"
"""

import "../../src/imseqs.nim"
import "../../src/types.nim"

# Objective: Ensure that type simplification works correctly for non-trivial types.
block:
  let primitives1 = new_immutable_seq([real_type, int_type, new_sum_type([int_type, string_type]), new_sum_type([boolean_type])])
  let primitives2 = new_immutable_seq([int_type, real_type, new_intersection_type([int_type, string_type]), new_sum_type([boolean_type])])
  let primitives3 = new_immutable_seq([int_type, real_type, new_intersection_type([int_type, string_type]), new_intersection_type([boolean_type])])
  let primitives4 = new_immutable_seq([int_type, real_type, new_sum_type([int_type, string_type]), new_intersection_type([boolean_type])])

  assert $sum_simplified(primitives1) == "(Real | Int | String | Boolean)"
  assert $sum_simplified(primitives2) == "(Int | Real | Boolean)"
  assert $intersection_simplified(primitives3) == "(Int & Real & String & Boolean)"
  assert $intersection_simplified(primitives4) == "(Int & Real & Boolean)"

  let tuples0 = new_immutable_seq([tuple_as_type([]), new_tuple_type([])])
  let tuples1 = new_immutable_seq([tuple_as_type([int_type]), new_tuple_type([intersection_as_type([int_type, real_type])])])
  let tuples234 = new_immutable_seq([
    tuple_as_type([int_type, int_type]),
    new_tuple_type([boolean_type, int_type, string_type, int_type]),
    new_tuple_type([int_type, int_type, real_type]),
    new_tuple_type([real_type, int_type]),
    new_tuple_type([real_type, int_type, real_type]),
    new_tuple_type([boolean_type, int_type, real_type, int_type]),
    new_tuple_type([int_type, string_type, real_type]),
  ])
  let tuples2569 = new_immutable_seq([
    tuple_as_type([int_type, int_type, int_type, int_type, int_type, real_type]),
    new_tuple_type([int_type, real_type]),
    new_tuple_type([int_type, int_type, real_type, int_type, int_type]),
    new_tuple_type([int_type, int_type, int_type, int_type, real_type, int_type, int_type, int_type, int_type]),
    new_tuple_type([int_type, real_type, int_type, int_type, int_type, int_type]),
    new_tuple_type([real_type, real_type]),
    new_tuple_type([int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type]),
    new_tuple_type([int_type, int_type, int_type, real_type, int_type, int_type]),
  ])

  assert $sum_simplified(tuples0) == "()"
  assert $sum_simplified(tuples1) == "(Int)"
  assert $intersection_simplified(tuples1) == "((Int & Real))"
  assert $sum_simplified(tuples234) == "(((Int | Real), Int) | ((Int | Real), (Int | String), Real) | (Boolean, Int, (String | Real), Int))"
  assert $intersection_simplified(tuples234) == "(((Int & Real), Int) & ((Int & Real), (Int & String), Real) & (Boolean, Int, (String & Real), Int))"
  assert $sum_simplified(tuples2569) == "(((Int | Real), Real) | (Int, Int, Real, Int, Int) | (Int, (Int | Real), Int, (Int | Real), Int, (Real | Int)) | (Int, Int, Int, Int, (Real | Int), Int, Int, Int, Int))"
  assert $intersection_simplified(tuples2569) == "(((Int & Real), Real) & (Int, Int, Real, Int, Int) & (Int, (Int & Real), Int, (Int & Real), Int, (Real & Int)) & (Int, Int, Int, Int, (Real & Int), Int, Int, Int, Int))"

  let functions1 = new_immutable_seq([
    function_as_type(new_tuple_type([sum_as_type([int_type, real_type])]), new_sum_type([string_type, boolean_type])),
    new_function_type(new_tuple_type([real_type]), boolean_type),
    new_function_type(new_tuple_type([int_type]), string_type),
  ])

  assert $sum_simplified(functions1) == "(((Real & Int)) => (String | Boolean))"
  assert $intersection_simplified(functions1) == "(((Int | Real)) => (Boolean & String))"

  let lists1 = new_immutable_seq([
    list_as_type(new_tuple_type([int_type, real_type, real_type])),
    list_as_type(new_tuple_type([boolean_type])),
    list_as_type(new_tuple_type([int_type, int_type, string_type])),
  ])

  assert $sum_simplified(lists1) == "[((Boolean) | (Int, (Real | Int), (Real | String)))]"
  assert $intersection_simplified(lists1) == "[((Boolean) & (Int, (Real & Int), (Real & String)))]"

  let meta_shape1 = get_meta_shape_safe(@["foo"])
  let meta_shape2 = get_meta_shape_safe(@["bar", "baz"])
  let meta_shape3 = get_meta_shape_safe(@["baz"])
  let shapes1 = new_immutable_seq([
    shape_as_type(meta_shape1, [real_type]),
    shape_as_type(meta_shape1, [string_type]),
    shape_as_type(meta_shape2, [string_type, int_type]),
    shape_as_type(meta_shape3, [real_type]),
  ])

  assert $sum_simplified(shapes1) == "(%{ foo: Real } | %{ foo: String } | %{ bar: String, baz: Int } | %{ baz: Real })"
  assert $intersection_simplified(shapes1) == "%{ bar: String, baz: (Int & Real), foo: (Real & String) }"

  # TODO (vm): Test declared type simplification.
