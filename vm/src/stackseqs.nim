# TODO (vm/poly): We should write some tests for StackSeq that verify stack/heap boundary behavior and edge cases.
type StackSeq*[I: static int, T] = object
  ## A StackSeq is a growable sequence that is initially allocated on the stack (up to a maximum of `I`). Once its
  ## elements don't fit the stack anymore, an array is allocated on the heap to hold the remaining elements.
  ##
  ## StackSeqs should rarely, if ever, be passed by value. For simplicity, the array allocated on the heap will never
  ## be freed if the sequence shrinks. Its intended use is locally in algorithms which want to avoid allocations for
  ## small amounts of data.
  ##
  ## A StackSeq is immediately valid when allocated in zeroed memory.
  len*: int
  heap_data*: ptr UncheckedArray[T]
  heap_cap*: int
  stack_data*: array[I, T]

proc `[]`*[I, T](seq: var StackSeq[I, T], index: int): T =
  if index < I:
    seq.stack_data[index]
  else:
    seq.heap_data[index - I]

proc `[]=`*[I, T](seq: var StackSeq[I, T], index: int, value: T) =
  if index < I:
    seq.stack_data[index] = value
  else:
    seq.heap_data[index] = value

iterator items*[I, T](seq: StackSeq[I, T]): T {.inline.} =
  ## The iterator first iterates through all stack items and then through all heap items. This implementation does less
  ## work compared to an implementation which naively uses the `[]` operator.
  let length = seq.len

  block:
    var i = 0
    let stack_length = min(length, I)
    while i < stack_length:
      yield seq.stack_data[i]
      i += 1

  if length > I:
    var i = 0
    let heap_length = length - I
    while i < heap_length:
      yield seq.heap_data[i]
      i += 1

proc grow[I, T](seq: var StackSeq[I, T], required_length: int) =
  if required_length >= I:
    let required_heap_cap = required_length - I
    if required_heap_cap > seq.heap_cap:
      # The division and multiplication ensure that `heap_cap` is a multiple of I.
      let new_heap_cap = ((required_heap_cap div I) + 1) * I
      var new_heap_data = cast[ptr UncheckedArray[T]](alloc(new_heap_cap * sizeof(T)))
      copy_mem(new_heap_data, seq.heap_data, seq.heap_cap * sizeof(T))
      seq.heap_data = new_heap_data
      seq.heap_cap = new_heap_cap

proc add*[I, T](seq: var StackSeq[I, T], value: T) =
  if seq.len < I:
    seq.stack_data[seq.len] = value
  else:
    let heap_index = seq.len - I
    if heap_index >= seq.heap_cap:
      grow(seq, seq.len + 1)
    seq.heap_data[heap_index] = value
  seq.len += 1

proc clear*[I, T](seq: var StackSeq[I, T]) =
  seq.len = 0
