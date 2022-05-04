import std/hashes

from utils import offset_addr

type
  ImSeq*[T] = ref ImSeqObj[T]
    ## An ImSeq is an immutable sequence. It has shallow copying semantics and should thus be preferred over Nim's
    ## `seq` when the elements are known exactly at sequence construction. Its `elements` are embedded in the object,
    ## so that there is only need for a single allocation.
    ##
    ## Mutation of an immutable sequence is possible and ONLY allowed for *fresh* ImSeqs which have just been created
    ## or copied. An immutable sequence which has already left its creator's scope, such as an object property, must
    ## never be mutated.

  ImSeqObj[T] = object
    len*: int
    elements*: UncheckedArray[T]

  FixedSeq*[T] = ImSeq[T]
    ## A FixedSeq is an ImSeq with no immutability guarantees. By contract, a FixedSeq may be mutated at any time and
    ## its owner should treat it as such.

proc alloc_immutable_seq[T](length: int): ImSeq[T] =
  # Note that `sizeof` of an unchecked array is 0, so we can use `sizeof(ImSeqObj[T])` to get the size of the preamble.
  let seq = cast[ImSeq[T]](alloc0(sizeof(ImSeqObj[T]) + length * sizeof(T)))
  seq.len = length
  seq

let empty = alloc_immutable_seq[uint64](0)

proc empty_immutable_seq*[T](): ImSeq[T] {.inline.} = cast[ImSeq[T]](empty)
  ## Returns an empty immutable sequence for the given element type. This is backed by a single constant ImSeq so that
  ## this function makes no allocations.

proc new_immutable_seq*[T](length: int): ImSeq[T] =
  ## Creates a new immutable sequence of the given length with zeroed entries. This can be used to efficiently
  ## initialize an immutable sequence. If `length` is 0, the empty sequence is returned.
  if length == 0: empty_immutable_seq[T]()
  else: alloc_immutable_seq[T](length)

proc new_immutable_seq*[T](length: uint): ImSeq[T] = new_immutable_seq[T](int(length))

proc new_immutable_seq[T](source: pointer, length: int): ImSeq[T] =
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, source, length * sizeof(T))
  seq

proc new_immutable_seq*[T](source: ptr UncheckedArray[T], length: int): ImSeq[T] = new_immutable_seq[T](cast[pointer](source), length)

proc new_immutable_seq*[T](source: open_array[T]): ImSeq[T] = new_immutable_seq[T](unsafe_addr source, source.len)

proc new_immutable_seq*[T](source: open_array[T], length: int): ImSeq[T] =
  ## Creates a new immutable sequence from `source`, but taking only the first `length` elements.
  new_immutable_seq[T](unsafe_addr source, length)

proc new_immutable_seq*[T](source: ImSeq[T]): ImSeq[T] =
  ## Copies all elements of `source` into a new immutable sequence with the same length. A copied immutable sequence is
  ## guaranteed to be fresh and may thus be mutated for initialization.
  new_immutable_seq[T](addr source.elements, source.len)

proc `[]`*[T](seq: ImSeq[T], index: int): T {.inline.} = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: int64): T {.inline.} = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: uint): T {.inline.} = seq.elements[index]

proc `[]=`*[T](seq: var ImSeq[T], index: int, value: T) =
  ## Replaces the sequence's element at the given index. This is an unsafe operation, as ImSeqs should be immutable,
  ## but it may be used to optimize initialization.
  seq.elements[index] = value

proc `[]=`*[T](seq: var ImSeq[T], index: uint, value: T) =
  seq.elements[index] = value

template to_open_array*[T](seq: ImSeq[T]): open_array[T] =
  to_open_array(addr seq.elements, 0, seq.len - 1)

iterator items*[T](seq: ImSeq[T]): T {.inline.} =
  var i = 0
  let length = seq.len
  while i < length:
    yield seq[i]
    i += 1

proc append*[T](seq: ImSeq[T], element: T): ImSeq[T] =
  let old_length = seq.len
  var new_seq = new_immutable_seq[T](old_length + 1)
  copy_mem(addr new_seq.elements, addr seq.elements, old_length * sizeof(T))
  new_seq[old_length] = element
  new_seq

template flatten_it*(seqs, element_type, operation: untyped): untyped =
  ## Flattens `seqs`. For every element of `seqs`, `operation` must return the corresponding ImSeq. Inside `operation`,
  ## `it` refers to each element sequence.
  var total_length = 0
  for element in seqs:
    let it {.inject.} = element
    total_length += operation.len
  var new_seq = new_immutable_seq[element_type](total_length)

  # Copy the given sequences into the new sequence by keeping a running offset.
  var byte_offset = 0
  for element in seqs:
    let it {.inject.} = element
    let seq = operation
    let byte_size = seq.len * sizeof(element_type)
    copy_mem(offset_addr(new_seq.elements, byte_offset), addr seq.elements, byte_size)
    byte_offset += byte_size
  new_seq

proc flatten*[T](seqs: open_array[ImSeq[T]]): ImSeq[T] = flatten_it(seqs, T, it)

proc flatten*[T](seqs: ImSeq[ImSeq[T]]): ImSeq[T] = flatten(seqs.to_open_array)

proc concat*[T](s1: ImSeq[T], s2: ImSeq[T]): ImSeq[T] = flatten([s1, s2])

proc slice*[T](seq: ImSeq[T], start: int, length: int): ImSeq[T] =
  ## Creates a slice of `seq` from index `start`, taking `length` elements if available.
  let new_length = min(seq.len - start, length)
  var new_seq = new_immutable_seq[T](new_length)
  copy_mem(addr new_seq.elements, offset_addr(seq.elements, start * sizeof(T)), new_length * sizeof(T))
  new_seq

template map_it*[T](seq: ImSeq[T], result_type: untyped, operation: untyped): untyped =
  ## Maps `seq` with `operation`. Inside `operation`, `it` refers to each element.
  var new_seq = new_immutable_seq[result_type](seq.len)
  for i in 0 ..< seq.len:
    let it {.inject.} = seq[i]
    new_seq[i] = operation
  new_seq

template flat_map_it*[T](seq: ImSeq[T], result_type: untyped, operation: untyped): untyped =
  ## Flat maps `seq` with `operation`. Inside `operation`, `it` refers to each element.
  var result_seqs = new_immutable_seq[ImSeq[result_type]](seq.len)
  for i in 0 ..< seq.len:
    let it {.inject.} = seq[i]
    result_seqs[i] = operation
  result_seqs.flatten()

template filter_it*[T](seq: ImSeq[T], predicate: untyped): untyped =
  ## Filters `seq` with `predicate`. Inside `predicate`, `it` refers to each element.
  var buffer = new_seq_of_cap[T](seq.len)
  for element in seq:
    let it {.inject.} = element
    if predicate:
      buffer.add(element)
  new_immutable_seq(buffer)

proc join*[T](seq: ImSeq[T], separator: string): string =
  let length = seq.len
  if length == 0:
    return ""

  var str = $seq[0]
  for i in 1 ..< length:
    str.add(separator)
    str.add($seq[i])
  str

proc `===`*[T](s1: ImSeq[T], s2: ImSeq[T]): bool {.inline.} = cast[pointer](s1) == cast[pointer](s2)
proc `!==`*[T](s1: ImSeq[T], s2: ImSeq[T]): bool {.inline.} = not (s1 === s2)

proc `==`*[T](s1: ImSeq[T], s2: ImSeq[T]): bool {.inline.} = s1 === s2 or s1.to_open_array == s2.to_open_array
proc `!=`*[T](s1: ImSeq[T], s2: ImSeq[T]): bool {.inline.} = not (s1 == s2)

proc `hash`*[T](seq: ImSeq[T]): Hash {.inline.} = hash(to_open_array(seq))

proc `<`*[T](s1: ImSeq[T], s2: ImSeq[T]): bool =
  ## Whether `s1` is lexicographically less than `s2`. If all common elements of `s1` and `s2` are equal, the shorter
  ## list is less than the other.
  let length = min(s1.len, s2.len)
  for i in 0 ..< length:
    if s1[i] < s2[i]:
      return true
    if s2[i] < s1[i]:
      return false
  s1.len < s2.len

proc `$`*[T](seq: ImSeq[T]): string =
  "[" & seq.join(", ") & "]"
