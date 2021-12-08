discard """
 matrix: "--gc:boehm"
"""

import "../../src/stackseqs.nim"

# Objective: StackSeqs should correctly initialize, grow, return the correct elements, and their iteration should lead
# to the correct sum.
var seq: StackSeq[8, int]
for i in 1 .. 8:
  seq.add(i)

assert seq.heap_data == nil
assert seq.heap_cap == 0

for i in 0 ..< 8:
  assert seq[i] == i + 1

# This should force the StackSeq to grow into the heap.
seq.add(42)

assert seq.heap_data != nil
assert seq.heap_cap == 8
assert seq[8] == 42

# And the cap should be raised again when we add another 12 elements.
for i in 0 ..< 12:
  seq.add(1)

assert seq.len == 21
assert seq.heap_cap == 16
for i in 9 .. 20:
  assert seq[i] == 1

# Finally, we can test iteration by summing all values.
var sum = 0
for n in seq:
  sum += n

assert sum == 90
