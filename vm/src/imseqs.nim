type
  ## An ImSeq is an immutable sequence. It has shallow copying semantics and should thus be preferred over Nim's `seq`
  ## when the elements are known exactly at sequence construction. Its `elements` are embedded in the object, so that
  ## there is only need for a single allocation.
  ImSeq*[T] = ref ImSeqObj[T]

  ImSeqObj[T] = object
    len*: int
    elements*: UncheckedArray[T]

proc alloc_immutable_seq[T](length: int): ImSeq[T] =
  # Note that `sizeof` of an unchecked array is 0, so we can use `sizeof(ImSeqObj[T])` to get the size of the preamble.
  let seq = cast[ImSeq[T]](alloc0(sizeof(ImSeqObj[T]) + length * sizeof(T)))
  seq.len = length
  seq

let empty = alloc_immutable_seq[uint64](0)

## Returns an empty immutable sequence for the given element type. This is backed by a single object reference so that
## reallocations do not have to occur.
proc empty_immutable_seq*[T](): ImSeq[T] = cast[ImSeq[T]](empty)

## Creates a new immutable sequence of the given length with zeroed entries. This can be used to efficiently initialize
## an immutable sequence. If `length` is 0, the empty sequence is returned.
proc new_immutable_seq*[T](length: int): ImSeq[T] =
  if length == 0: empty_immutable_seq[T]()
  else: alloc_immutable_seq[T](length)

proc new_immutable_seq*[T](length: uint): ImSeq[T] = new_immutable_seq[T](int(length))

proc new_immutable_seq*[T](source: open_array[T]): ImSeq[T] =
  let length = source.len
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, unsafe_addr source, length * sizeof(T))
  seq

## Creates a new immutable sequence from `source`, but taking only the first `length` elements.
proc new_immutable_seq*[T](source: open_array[T], length: int): ImSeq[T] =
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, unsafe_addr source, length * sizeof(T))
  seq

proc new_immutable_seq*[T](source: ImSeq[T]): ImSeq[T] =
  let length = source.len
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, addr source.elements, length * sizeof(T))
  seq

proc `[]`*[T](seq: ImSeq[T], index: int): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: int64): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: uint): T = seq.elements[index]

## Replaces the sequence's element at the given index. This is an unsafe operation, as ImSeqs should be immutable, but
## may be used to optimize initialization.
proc `[]=`*[T](seq: var ImSeq[T], index: int, value: T) =
  seq.elements[index] = value

proc `[]=`*[T](seq: var ImSeq[T], index: uint, value: T) =
  seq.elements[index] = value

template to_open_array*[T](seq: ImSeq[T]): open_array[T] =
  to_open_array(addr seq.elements, 0, seq.len)

proc append*[T](old_seq: ImSeq[T], element: T): ImSeq[T] =
  let old_length = old_seq.len
  var new_seq = new_immutable_seq[T](old_length + 1)
  copy_mem(addr new_seq.elements, addr old_seq.elements, old_length * sizeof(T))
  new_seq[old_length] = element
  new_seq

iterator items*[T](seq: ImSeq[T]): T {.inline.} =
  var i = 0
  let length = seq.len
  while i < length:
    yield seq[i]
    i += 1

proc join*[T](seq: ImSeq[T], separator: string): string =
  let length = seq.len
  if length == 0:
    return ""

  var str = $seq[0]
  for i in 1 ..< length:
    str.add(separator)
    str.add($seq[i])
  str

proc `$`*[T](seq: ImSeq[T]): string =
  "ImSeq[" & seq.join(", ") & "]"
