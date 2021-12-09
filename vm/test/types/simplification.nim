discard """
 matrix: "--gc:boehm"
"""

import "../../src/imseqs.nim"
import "../../src/types.nim"

# Objective: Ensure that type simplification works correctly for non-trivial types.
let primitives1 = new_immutable_seq([real_type, int_type, sum([int_type, string_type]), sum([boolean_type])])
let primitives2 = new_immutable_seq([int_type, real_type, intersection([int_type, string_type]), sum([boolean_type])])
let primitives3 = new_immutable_seq([int_type, real_type, intersection([int_type, string_type]), intersection([boolean_type])])
let primitives4 = new_immutable_seq([int_type, real_type, sum([int_type, string_type]), intersection([boolean_type])])

assert $sum_simplified(primitives1) == "(Real | Int | String | Boolean)"
assert $sum_simplified(primitives2) == "(Int | Real | Boolean)"
assert $intersection_simplified(primitives3) == "(Int & Real & String & Boolean)"
assert $intersection_simplified(primitives4) == "(Int & Real & Boolean)"

let tuples0 = new_immutable_seq([tpl_as_type([]), tpl([])])
let tuples1 = new_immutable_seq([tpl_as_type([int_type]), tpl([intersection_as_type([int_type, real_type])])])
let tuples234 = new_immutable_seq([
  tpl_as_type([int_type, int_type]),
  tpl([boolean_type, int_type, string_type, int_type]),
  tpl([int_type, int_type, real_type]),
  tpl([real_type, int_type]),
  tpl([real_type, int_type, real_type]),
  tpl([boolean_type, int_type, real_type, int_type]),
  tpl([int_type, string_type, real_type]),
])
let tuples2569 = new_immutable_seq([
  tpl_as_type([int_type, int_type, int_type, int_type, int_type, real_type]),
  tpl([int_type, real_type]),
  tpl([int_type, int_type, real_type, int_type, int_type]),
  tpl([int_type, int_type, int_type, int_type, real_type, int_type, int_type, int_type, int_type]),
  tpl([int_type, real_type, int_type, int_type, int_type, int_type]),
  tpl([real_type, real_type]),
  tpl([int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type, int_type]),
  tpl([int_type, int_type, int_type, real_type, int_type, int_type]),
])

assert $sum_simplified(tuples0) == "()"
assert $sum_simplified(tuples1) == "(Int)"
assert $intersection_simplified(tuples1) == "((Int & Real))"
assert $sum_simplified(tuples234) == "(((Int | Real), Int) | ((Int | Real), (Int | String), Real) | (Boolean, Int, (String | Real), Int))"
assert $intersection_simplified(tuples234) == "(((Int & Real), Int) & ((Int & Real), (Int & String), Real) & (Boolean, Int, (String & Real), Int))"
assert $sum_simplified(tuples2569) == "(((Int | Real), Real) | (Int, Int, Real, Int, Int) | (Int, (Int | Real), Int, (Int | Real), Int, (Real | Int)) | (Int, Int, Int, Int, (Real | Int), Int, Int, Int, Int))"
assert $intersection_simplified(tuples2569) == "(((Int & Real), Real) & (Int, Int, Real, Int, Int) & (Int, (Int & Real), Int, (Int & Real), Int, (Real & Int)) & (Int, Int, Int, Int, (Real & Int), Int, Int, Int, Int))"

let functions1 = new_immutable_seq([
  function_as_type(tpl([sum_as_type([int_type, real_type])]), sum([string_type, boolean_type])),
  function(tpl([real_type]), boolean_type),
  function(tpl([int_type]), string_type),
])

assert $sum_simplified(functions1) == "(((Real & Int)) => (String | Boolean))"
assert $intersection_simplified(functions1) == "(((Int | Real)) => (Boolean & String))"

let lists1 = new_immutable_seq([
  list_as_type(tpl([int_type, real_type, real_type])),
  list_as_type(tpl([boolean_type])),
  list_as_type(tpl([int_type, int_type, string_type])),
])

assert $sum_simplified(lists1) == "[((Boolean) | (Int, (Real | Int), (Real | String)))]"
assert $intersection_simplified(lists1) == "[((Boolean) & (Int, (Real & Int), (Real & String)))]"
